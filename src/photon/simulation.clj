(ns photon.simulation
  "Network simulation engine.

   A network is a map:
     {:nodes   {id {:id id :level float :threshold float :active bool}}
      :edges   {to-id [{:from from-id :index int} ...]}  ;; sorted input list per node
      :rules   {id {:table {[bool ...] float}}}           ;; truth-table per node
      :step    int}

   Rules are truth tables: for a node with n inputs, the rule maps each
   possible boolean input vector (length n) to a level delta (float).
   Inputs are read in the order defined by :edges for that node."
  (:require [clojure.set]))

;; ---- expression tree rules ----
;;
;; Rules are random programs stored as Clojure data (S-expressions).
;; Terminals:
;;   [:input i]       — read input #i (true→1.0, false→0.0)
;;   [:const v]       — literal float
;; Boolean (treat >0.5 as true, emit 1.0 or 0.0):
;;   [:and a b]  [:or a b]  [:xor a b]  [:not a]
;; Arithmetic:
;;   [:+ a b]  [:- a b]  [:* a b]
;; Conditional:
;;   [:if cond then else]
;; Aggregate (subset of inputs):
;;   [:sum i1 i2 ...]    — count of active inputs in subset
;;   [:maj i1 i2 ...]    — majority vote (>half active → 1.0)
;;
;; Evaluated to a float, then: delta = tanh(result) * scale

(defn- rand-inputs
  "Pick k random input indices from [0, n)."
  [n k]
  (take k (shuffle (range n))))

(defn- gen-expr
  "Generate a random expression tree.
   depth: remaining depth budget.  n: number of available inputs."
  [depth n]
  (if (or (<= depth 0) (< (rand) 0.25))
    ;; terminal
    (if (and (pos? n) (< (rand) 0.7))
      [:input (rand-int n)]
      [:const (- (* 2.0 (rand)) 1.0)])
    ;; operator
    (let [ops (cond-> [:and :or :xor :not :+ :- :* :if :eq]
                (>= n 2) (conj :sum :maj :count-eq))
          op  (rand-nth ops)]
      (case op
        :not  [:not (gen-expr (dec depth) n)]
        :eq   [:eq (gen-expr (dec depth) n) (gen-expr (dec depth) n)]
        (:and :or :xor :+ :- :*)
              [op (gen-expr (dec depth) n) (gen-expr (dec depth) n)]
        :if   [:if (gen-expr (dec depth) n)
                   (gen-expr (dec depth) n)
                   (gen-expr (dec depth) n)]
        :sum  (into [:sum] (rand-inputs n (+ 2 (rand-int (min 5 n)))))
        :maj  (into [:maj] (rand-inputs n (+ 3 (rand-int (min 5 n)))))
        :count-eq (let [indices (rand-inputs n (+ 2 (rand-int (min 5 n))))
                        target  (rand-int (inc (count indices)))]
                    (into [:count-eq target] indices))))))

(defn- eval-expr
  "Evaluate an expression tree given a vector of boolean input states.
   Returns a float."
  [expr input-states]
  (let [bool->f (fn [b] (if b 1.0 0.0))
        f->bool (fn [v] (> v 0.5))
        ev      (fn ev [e]
                  (case (first e)
                    :input  (bool->f (nth input-states (second e) false))
                    :const  (double (second e))
                    :not    (bool->f (not (f->bool (ev (nth e 1)))))
                    :and    (bool->f (and (f->bool (ev (nth e 1)))
                                         (f->bool (ev (nth e 2)))))
                    :or     (bool->f (or  (f->bool (ev (nth e 1)))
                                         (f->bool (ev (nth e 2)))))
                    :xor    (bool->f (not= (f->bool (ev (nth e 1)))
                                           (f->bool (ev (nth e 2)))))
                    :+      (+ (ev (nth e 1)) (ev (nth e 2)))
                    :-      (- (ev (nth e 1)) (ev (nth e 2)))
                    :*      (* (ev (nth e 1)) (ev (nth e 2)))
                    :if     (if (f->bool (ev (nth e 1)))
                              (ev (nth e 2))
                              (ev (nth e 3)))
                    :sum    (reduce + 0.0
                              (map (fn [i] (bool->f (nth input-states i false)))
                                   (rest e)))
                    :eq     (bool->f (== (ev (nth e 1)) (ev (nth e 2))))
                    :maj    (let [indices (rest e)
                                 active  (count (filter #(nth input-states % false) indices))]
                              (bool->f (> active (/ (count indices) 2))))
                    :count-eq (let [target  (second e)
                                   indices (drop 2 e)
                                   active  (count (filter #(nth input-states % false) indices))]
                                (bool->f (== active target)))
                    ;; fallback
                    0.0))]
    (ev expr)))

(defn- random-rule
  "Generate a random expression-tree rule for a node with n inputs.
   Tree depth 3-5 gives rich behavior without being huge."
  [n delta-range]
  {:expr  (gen-expr (+ 3 (rand-int 3)) n)
   :scale delta-range})

(defn- connection-fingerprint
  "For each node, the set of all directed neighbors (both in and out)."
  [edges ids]
  (let [base (into {} (map (fn [id] [id #{}])) ids)]
    (reduce
     (fn [acc [to-id inputs]]
       (reduce
        (fn [acc2 {:keys [from]}]
          (-> acc2
              (update to-id conj from)
              (update from conj to-id)))
        acc
        inputs))
     base
     edges)))

(defn- jaccard [s1 s2]
  (if (and (empty? s1) (empty? s2))
    0.0
    (let [inter (count (clojure.set/intersection s1 s2))
          uni   (count (clojure.set/union s1 s2))]
      (/ (double inter) (double uni)))))

(defn- similarity-order
  "Order node ids so that adjacent nodes on the circle share the most
   connections. Greedy nearest-neighbor walk."
  [ids edges]
  (let [fps   (connection-fingerprint edges ids)
        start (first ids)]
    (loop [ordered [start]
           remaining (set (rest ids))]
      (if (empty? remaining)
        ordered
        (let [current  (peek ordered)
              cur-fp   (get fps current)
              best     (apply max-key
                              (fn [id] (jaccard cur-fp (get fps id)))
                              remaining)]
          (recur (conj ordered best)
                 (disj remaining best)))))))

(defn generate-network
  "Create a random network.
   - size:    number of nodes
   - density: probability [0,1] that any directed edge exists (self-loops excluded)
   Returns a complete network map ready for simulation."
  [size density]
  (let [ids       (range size)
        ;; generate nodes with random level in [0,2] and threshold in [0.3,1.7]
        nodes     (into {}
                        (map (fn [id]
                               [id {:id        id
                                    :level     (* 2.0 (rand))
                                    :threshold (+ 0.3 (* 1.4 (rand)))
                                    :active    false}]))
                        ids)
        ;; activate nodes whose initial level >= threshold
        nodes     (into {}
                        (map (fn [[id n]]
                               [id (assoc n :active (>= (:level n) (:threshold n)))]))
                        nodes)
        ;; generate directed edges based on density
        edges     (reduce
                   (fn [acc to-id]
                     (let [inputs (->> ids
                                       (remove #{to-id})
                                       (filter (fn [_] (< (rand) density)))
                                       (mapv (fn [from-id] {:from from-id}))
                                       ;; assign stable index order
                                       (map-indexed (fn [i e] (assoc e :index i)))
                                       vec)]
                       (assoc acc to-id inputs)))
                   {}
                   ids)
        ;; generate a rule for each node based on its input count
        rules     (into {}
                        (map (fn [id]
                               [id (random-rule (count (get edges id)) 1.0)]))
                        ids)]
    {:nodes nodes
     :edges edges
     :rules rules
     :step  0
     :node-order (similarity-order ids edges)}))

(defn- compute-delta
  "Compute the level delta for a single node given current network state."
  [{:keys [nodes edges rules]} node-id]
  (let [inputs (get edges node-id)
        {:keys [expr scale]} (get rules node-id)]
    (if (empty? inputs)
      0.0
      (let [input-states (mapv (fn [{:keys [from]}]
                                 (:active (get nodes from)))
                               inputs)
            raw          (eval-expr expr input-states)]
        (* scale (Math/tanh raw))))))

(defn step
  "Advance the simulation by one tick.
   1. Evaluate each node's expression tree on current input states.
   2. The expression output directly determines next activation (>0 = on).
   3. Level tracks a smoothed version for visual sizing.
   All outputs computed on current state, applied simultaneously."
  [network]
  (let [ids     (keys (:nodes network))
        outputs (into {}
                      (map (fn [id] [id (compute-delta network id)]))
                      ids)
        nodes'  (into {}
                      (map (fn [[id node]]
                             (let [raw       (get outputs id 0.0)
                                   new-active (> raw 0.0)
                                   ;; level smoothly tracks activation for visual sizing
                                   target    (if new-active 2.5 0.5)
                                   new-level (+ (:level node)
                                                (* 0.3 (- target (:level node))))]
                               [id (assoc node
                                          :level  new-level
                                          :active new-active)])))
                      (:nodes network))]
    (assoc network
           :nodes nodes'
           :step  (inc (:step network)))))

(defn network->client-data
  "Extract the data the client needs for rendering."
  [{:keys [nodes edges step node-order]}]
  {:step  step
   :nodes (mapv (fn [id]
                  (let [n (get nodes id)]
                    {:id        id
                     :level     (double (:level n))
                     :threshold (double (:threshold n))
                     :active    (:active n)}))
                node-order)
   :edges (into []
                (mapcat (fn [[to-id inputs]]
                          (map (fn [{:keys [from]}]
                                 {:from from :to to-id})
                               inputs)))
                edges)})
