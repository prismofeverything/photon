(ns photon.app
  (:require [reagent.core :as r]
            [reagent.dom.client :as rdc]
            [photon.ws :as ws]
            [photon.scene :as scene]))

;; ---- local UI state ----

(defonce ui-state (r/atom {:size    20
                           :density 0.3}))

;; ---- watch network changes to update Three.js scene ----

(defonce _watcher
  (add-watch ws/state ::scene-updater
    (fn [_ _ old new]
      (when (and (:network new) (not= (:network old) (:network new)))
        (let [old-net (:network old)
              new-net (:network new)
              ;; topology changed if edges differ or node count changed
              topo-changed? (or (nil? old-net)
                                (not= (:edges old-net) (:edges new-net))
                                (not= (count (:nodes old-net))
                                      (count (:nodes new-net))))]
          (if topo-changed?
            ;; full rebuild + set initial visual state
            (do (scene/build-network! new-net)
                (scene/push-state! new-net))
            ;; just animate to new state
            (scene/push-state! new-net)))))))

;; ---- components ----

(defn slider [{:keys [label value min max step on-change display]}]
  [:div.control
   [:label label " " [:span.value (or display value)]]
   [:input {:type      "range"
            :min       min
            :max       max
            :step      (or step 1)
            :value     value
            :on-change #(on-change (js/parseFloat (.. % -target -value)))}]])

(defn controls []
  (let [{:keys [size density]} @ui-state
        {:keys [connected running network]} @ws/state
        step-count (:step network)]
    [:div.controls
     [:div.header
      [:h1 "PHOTON"]
      [:span.status {:class (if connected "on" "off")}
       (if connected "connected" "disconnected")]]

     [slider {:label "Network Size"
              :value size :min 4 :max 80 :step 1
              :on-change #(swap! ui-state assoc :size (int %))}]

     [slider {:label "Connectivity"
              :value density :min 0.05 :max 1.0 :step 0.05
              :display (.toFixed density 2)
              :on-change #(swap! ui-state assoc :density %)}]

     (let [;; log scale: slider 0-1 maps to 50-5000ms tick rate
           ;; ms = 50 * 100^slider
           ms->slider (fn [ms] (/ (js/Math.log (/ ms 50)) (js/Math.log 100)))
           slider->ms (fn [s] (* 50 (js/Math.pow 100 s)))
           current-ms @scene/tick-ms]
       [slider {:label     "Speed"
                :value     (ms->slider current-ms)
                :min       0 :max 1 :step 0.01
                :display   (str (js/Math.round current-ms) "ms")
                :on-change (fn [v]
                             (let [ms (slider->ms v)]
                               (reset! scene/tick-ms ms)
                               (ws/send! {"action" "set-tick" "ms" (js/Math.round ms)})))}])

     [:div.buttons
      [:button.btn.generate
       {:on-click #(ws/send! {"action" "generate"
                              "size"   (:size @ui-state)
                              "density" (:density @ui-state)})}
       "Generate"]

      (if running
        [:button.btn.stop
         {:on-click #(ws/send! {"action" "stop"})}
         "Stop"]
        [:button.btn.start
         {:on-click #(ws/send! {"action" "start"})}
         "Start"])

      [:button.btn.step
       {:on-click #(ws/send! {"action" "step"})
        :disabled running}
       "Step"]]

     (when network
       [:div.info
        [:div "Step: " [:span.value step-count]]
        [:div "Nodes: " [:span.value (count (:nodes network))]]
        [:div "Edges: " [:span.value (count (:edges network))]]
        (let [active (count (filter :active (:nodes network)))]
          [:div "Active: " [:span.value active]
           " / " (count (:nodes network))])])]))

(defn node-table []
  (let [{:keys [network]} @ws/state]
    (when network
      [:div.node-table
       [:h3 "Node States"]
       [:div.table-scroll
        [:table
         [:thead [:tr [:th "ID"] [:th "Level"] [:th "Thr"] [:th "Active"]]]
         [:tbody
          (for [node (:nodes network)]
            ^{:key (:id node)}
            [:tr {:class (when (:active node) "active")}
             [:td (:id node)]
             [:td (.toFixed (:level node) 2)]
             [:td (.toFixed (:threshold node) 2)]
             [:td (if (:active node) "ON" "off")]])]]]])))

(defn app []
  [:div.app
   [:div.sidebar
    [controls]
    [node-table]]
   [:div.viewport {:ref (fn [el]
                          (when (and el (not @scene/scene-state))
                            (scene/setup! el)))}]])

;; ---- lifecycle ----

(defonce root (atom nil))

(defn ^:dev/after-load reload! []
  (when-let [r @root]
    (.render r (r/as-element [app]))))

(defn init! []
  (ws/connect!)
  (reset! root (rdc/create-root (js/document.getElementById "root")))
  (reload!))
