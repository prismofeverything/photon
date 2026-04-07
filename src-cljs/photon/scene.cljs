(ns photon.scene
  "Three.js scene for network visualization with animated state transitions.
   Nodes in a circle, connections as 3D arcs with directional arrows.
   Each tick: a gradient texture slides along each edge to show activation
   sweeping from source to target. Nodes smoothly lerp between on/off."
  (:require ["three" :as THREE]))

(set! *warn-on-infer* false)

(defonce scene-state (atom nil))

;; ---- colors ----

(def color-on      (THREE/Color. 0x00ffaa))
(def color-off     (THREE/Color. 0x1a3366))
(def emissive-on   (THREE/Color. 0x00ff88))
(def emissive-off  (THREE/Color. 0x112255))

;; CSS color strings for canvas drawing
(def edge-active-css   "#00ffaa")
(def edge-inactive-css "#183060")

(def ^:const seg-count 48)   ;; curve sample points

;; ---- geometry helpers ----

(defn- node-position [i n radius]
  (let [angle (* 2 js/Math.PI (/ i n))]
    (THREE/Vector3. (* radius (js/Math.cos angle))
                    0
                    (* radius (js/Math.sin angle)))))

(defn- create-arc-curve
  "Create a bezier arc from p1→p2. `lateral` shifts the control point
   perpendicular to the p1-p2 line in the XZ plane.
   The perpendicular naturally reverses when p1/p2 swap, so A→B and B→A
   with the same positive lateral will curve to opposite sides."
  [p1 p2 height lateral]
  (let [dx     (- (.-x p2) (.-x p1))
        dz     (- (.-z p2) (.-z p1))
        len    (js/Math.sqrt (+ (* dx dx) (* dz dz)))
        perp-x (if (pos? len) (/ (- dz) len) 0)
        perp-z (if (pos? len) (/ dx len) 0)
        mid-x  (+ (* 0.5 (+ (.-x p1) (.-x p2))) (* lateral perp-x))
        mid-y  (+ (* 0.5 (+ (.-y p1) (.-y p2))) height)
        mid-z  (+ (* 0.5 (+ (.-z p1) (.-z p2))) (* lateral perp-z))
        ctrl   (THREE/Vector3. mid-x mid-y mid-z)]
    (THREE/QuadraticBezierCurve3. p1 ctrl p2)))

(defn- ease-smooth [t]
  (let [t (max 0.0 (min 1.0 t))]
    (* t t (- 3.0 (* 2.0 t)))))

;; ---- gradient texture creation ----

(defn- make-gradient-texture
  "Create a 512x1 canvas texture with a gradient.
   The texture is 2x wide: left half is solid new-color,
   right half is solid old-color, with a soft gradient between them.
   We map UVs to the right half [0.5-1.0] initially (showing old-color),
   then slide offset.x from -0.5 to 0 so the new-color sweeps across."
  [new-css old-css]
  (let [canvas (js/document.createElement "canvas")
        ctx    (.getContext canvas "2d")
        w      512]
    (set! (.-width canvas) w)
    (set! (.-height canvas) 1)
    (let [grad (.createLinearGradient ctx 0 0 w 0)]
      ;; left side: solid old color
      (.addColorStop grad 0.0  old-css)
      (.addColorStop grad 0.5  old-css)
      ;; soft gradient in middle
      (.addColorStop grad 0.6  new-css)
      ;; right side: solid new color
      (.addColorStop grad 1.0  new-css)
      (set! (.-fillStyle ctx) grad)
      (.fillRect ctx 0 0 w 1))
    (let [tex (THREE/CanvasTexture. canvas)]
      (set! (.-wrapS tex) THREE/ClampToEdgeWrapping)
      (set! (.-wrapT tex) THREE/ClampToEdgeWrapping)
      (set! (.-magFilter tex) THREE/LinearFilter)
      (set! (.-minFilter tex) THREE/LinearFilter)
      tex)))

(defn- make-solid-texture
  "Create a 4x1 solid-color canvas texture."
  [css-color]
  (let [canvas (js/document.createElement "canvas")
        ctx    (.getContext canvas "2d")]
    (set! (.-width canvas) 4)
    (set! (.-height canvas) 1)
    (set! (.-fillStyle ctx) css-color)
    (.fillRect ctx 0 0 4 1)
    (let [tex (THREE/CanvasTexture. canvas)]
      (set! (.-wrapS tex) THREE/ClampToEdgeWrapping)
      (set! (.-wrapT tex) THREE/ClampToEdgeWrapping)
      tex)))

;; pre-build the 4 textures we need
(defonce tex-solid-active   (make-solid-texture edge-active-css))
(defonce tex-solid-inactive (make-solid-texture edge-inactive-css))
(defonce tex-activate       (make-gradient-texture edge-active-css edge-inactive-css))
(defonce tex-deactivate     (make-gradient-texture edge-inactive-css edge-active-css))

;; ---- build helpers ----

(defn- build-node-mesh []
  (let [geo (THREE/SphereGeometry. 0.15 16 16)
        mat (THREE/MeshStandardMaterial.
             #js {:color     0x1a3366
                  :emissive  0x112255
                  :emissiveIntensity 0.5
                  :metalness 0.3
                  :roughness 0.4})]
    (THREE/Mesh. geo mat)))

(defn- build-label-sprite [text]
  (let [canvas (js/document.createElement "canvas")
        ctx    (.getContext canvas "2d")]
    (set! (.-width canvas) 128)
    (set! (.-height canvas) 64)
    (set! (.-font ctx) "bold 28px monospace")
    (set! (.-fillStyle ctx) "#88ccff")
    (set! (.-textAlign ctx) "center")
    (set! (.-textBaseline ctx) "middle")
    (.fillText ctx text 64 32)
    (let [tex    (THREE/CanvasTexture. canvas)
          mat    (THREE/SpriteMaterial. #js {:map tex :transparent true :depthTest false})
          sprite (THREE/Sprite. mat)]
      (.set (.-scale sprite) 0.5 0.25 1)
      sprite)))

(defn- build-edge-objects
  "Build a textured line (via MeshLine-style tube) and arrowhead for one edge.
   We use a TubeGeometry so we can apply a texture with UVs.
   `lateral` offsets the arc perpendicular to the edge so bidirectional
   edges don't overlap. `perp-sign` fixes the perpendicular direction.
   Returns {:tube :arrow :curve :material}."
  [p1 p2 lateral]
  (let [dx    (- (.-x p2) (.-x p1))
        dz    (- (.-z p2) (.-z p1))
        dist  (js/Math.sqrt (+ (* dx dx) (* dz dz)))
        h     (* 0.3 dist)
        curve (create-arc-curve p1 p2 h lateral)
        ;; TubeGeometry gives us a mesh with proper UVs
        ;; (radius, tubularSegments, radialSegments)
        tube-geo (THREE/TubeGeometry. curve seg-count 0.02 4 false)
        tube-mat (THREE/MeshBasicMaterial.
                  #js {:map          (.clone tex-solid-inactive)
                       :transparent  true
                       :opacity      0.9
                       :side         THREE/DoubleSide})
        tube     (THREE/Mesh. tube-geo tube-mat)
        ;; arrowhead at 85%
        arrow-t   0.85
        arrow-pos (.getPointAt curve arrow-t)
        tangent   (.getTangentAt curve arrow-t)
        cone-geo  (THREE/ConeGeometry. 0.06 0.18 6)
        cone-mat  (THREE/MeshStandardMaterial.
                   #js {:color     0x183060
                        :emissive  0x183060
                        :emissiveIntensity 0.4
                        :transparent true
                        :opacity   0.8})
        cone      (THREE/Mesh. cone-geo cone-mat)
        up        (THREE/Vector3. 0 1 0)
        quat      (THREE/Quaternion.)]
    (.setFromUnitVectors quat up (.normalize tangent))
    (.copy (.-position cone) arrow-pos)
    (.setFromQuaternion (.-rotation cone) quat)
    {:tube tube :arrow cone :curve curve :material tube-mat}))

;; ---- build full network ----

(defn build-network!
  "Create all persistent Three.js objects for the network topology."
  [network-data]
  (when-let [{:keys [node-group edge-group]} @scene-state]
    (let [{:keys [nodes edges]} network-data
          n      (count nodes)
          radius (max 2 (* 0.3 n))]
      ;; clear
      (while (pos? (.-length (.-children node-group)))
        (.remove node-group (aget (.-children node-group) 0)))
      (while (pos? (.-length (.-children edge-group)))
        (.remove edge-group (aget (.-children edge-group) 0)))
      ;; positions
      (let [positions (into {}
                            (map-indexed
                             (fn [i node]
                               [(:id node) (node-position i n radius)]))
                            nodes)
            ;; nodes
            node-objs (into {}
                            (map (fn [{:keys [id]}]
                                   (let [mesh  (build-node-mesh)
                                         label (build-label-sprite (str id))
                                         pos   (get positions id)]
                                     (.copy (.-position mesh) pos)
                                     (.copy (.-position label) pos)
                                     (set! (.. label -position -y) 0.35)
                                     (.add node-group mesh)
                                     (.add node-group label)
                                     [id {:mesh mesh :label label}])))
                            nodes)
            ;; build set of edge pairs to detect bidirectional
            edge-set  (into #{} (map (fn [{:keys [from to]}] [from to])) edges)
            ;; edges
            edge-objs (into []
                            (keep (fn [{:keys [from to]}]
                                    (when (and (get positions from) (get positions to))
                                      (let [p1      (get positions from)
                                            p2      (get positions to)
                                            bidir?  (contains? edge-set [to from])
                                            dx      (- (.-x p2) (.-x p1))
                                            dz      (- (.-z p2) (.-z p1))
                                            dist    (js/Math.sqrt (+ (* dx dx) (* dz dz)))
                                            ;; same positive lateral for both directions —
                                            ;; the perpendicular naturally flips when
                                            ;; p1/p2 swap, pushing arcs to opposite sides
                                            lateral   (if bidir? (* 0.3 dist) 0.0)
                                            objs      (build-edge-objects p1 p2 lateral)]
                                        (.add edge-group (:tube objs))
                                        (.add edge-group (:arrow objs))
                                        (assoc objs :from from :to to)))))
                            edges)]
        ;; auto-fit camera to network size
        ;; need enough distance so the full circle fits in the 60° FOV
        ;; from any angle: radius / tan(30°) ≈ radius * 1.73, plus margin
        (when-let [ms (:mouse-state @scene-state)]
          (let [cam-radius (+ (* radius 2.0) 3)]
            (swap! ms assoc :radius cam-radius)))
        (swap! scene-state assoc
               :node-objs  node-objs
               :edge-objs  edge-objs
               :positions  positions
               :prev-nodes (into {} (map (fn [n] [(:id n) (select-keys n [:active :level])])) nodes)
               :curr-nodes (into {} (map (fn [n] [(:id n) (select-keys n [:active :level])])) nodes)
               :tick-start nil)))))

;; ---- receive new state ----

(defonce pending-state (atom nil))

(defonce tick-ms (atom 600))   ;; total tick interval (synced with server)
(def anim-ratio 0.8)          ;; animation fills 80% of tick

(defn- apply-pending-state!
  "Take whatever is queued in pending-state and start animating to it."
  []
  (when-let [new-active @pending-state]
    (reset! pending-state nil)
    (when-let [{:keys [edge-objs curr-nodes]} @scene-state]
      (when (seq edge-objs)
        (let [prev curr-nodes]
          ;; assign the right texture to each edge for this transition
          (doseq [{:keys [material from]} edge-objs]
            (let [was (:active (get prev from))
                  now (:active (get new-active from))
                  tex (cond
                        (and (not was) now)  (.clone tex-activate)
                        (and was (not now))  (.clone tex-deactivate)
                        now                  (.clone tex-solid-active)
                        :else                (.clone tex-solid-inactive))]
              (set! (.-wrapS tex) THREE/ClampToEdgeWrapping)
              ;; tube UVs 0→1 sample a half-width window via repeat.x
              ;; start at offset 0.5 (old-color half), slide to 0.0 (new-color half)
              (.set (.-repeat tex) 0.5 1)
              (set! (.. tex -offset -x) 0.0)
              (set! (.-map material) tex)
              (set! (.-needsUpdate material) true)))
          (swap! scene-state assoc
                 :prev-nodes prev
                 :curr-nodes new-active
                 :tick-start (js/performance.now)))))))

(defn push-state!
  "Receive new simulation state. Queue it — only start animating when
   the current animation is done (or if there's no animation running)."
  [network-data]
  (let [new-nodes (into {} (map (fn [n] [(:id n) (select-keys n [:active :level])]))
                        (:nodes network-data))]
    (reset! pending-state new-nodes)
    ;; if no animation running, start immediately
    (when-let [{:keys [tick-start]} @scene-state]
      (when (nil? tick-start)
        (apply-pending-state!)))))

;; ---- per-frame animation ----

(defn- animate-edges!
  "Slide each edge's gradient texture from source→target."
  [{:keys [edge-objs prev-nodes curr-nodes]} progress]
  (let [t (ease-smooth progress)]
    (doseq [{:keys [material arrow from]} edge-objs]
      (let [src-was (:active (get prev-nodes from))
            src-now (:active (get curr-nodes from))
            changing? (not= src-was src-now)]
        ;; slide texture: offset goes from 0.5 → 0.0
        (when-let [tex (.-map material)]
          (if changing?
            (set! (.. tex -offset -x) (* 0.5 t))
            ;; for non-changing edges, just show solid color
            (set! (.. tex -offset -x) 0.0)))
        ;; arrow color: flip when wavefront passes 85%
        (let [arrow-mat  (.-material arrow)
              wavefront  t
              past?      (> wavefront 0.85)
              active-now (if past? src-now src-was)
              ac         (if active-now (THREE/Color. 0x00ffaa) (THREE/Color. 0x183060))]
          (.copy (.-color arrow-mat) ac)
          (.copy (.-emissive arrow-mat) ac)
          (set! (.-emissiveIntensity arrow-mat) (if active-now 0.8 0.4))
          (set! (.-needsUpdate arrow-mat) true))))))

(defn- animate-nodes!
  "Smoothly lerp node color/emissive over the full tick duration."
  [{:keys [node-objs prev-nodes curr-nodes]} progress]
  (let [t         (ease-smooth progress)
        tmp-color (THREE/Color.)]
    (doseq [[id {:keys [mesh]}] node-objs]
      (let [prev-n (get prev-nodes id)
            curr-n (get curr-nodes id)
            was    (:active prev-n)
            now    (:active curr-n)
            mat    (.-material mesh)
            from-v (if was 1.0 0.0)
            to-v   (if now 1.0 0.0)
            v      (+ from-v (* (- to-v from-v) t))
            ;; lerp level for smooth size transition
            prev-level (or (:level prev-n) 1.0)
            curr-level (or (:level curr-n) 1.0)
            level      (+ prev-level (* (- curr-level prev-level) t))
            ;; scale by sqrt(level) for area-proportional sizing
            ;; level range [0, 3], base scale 0.5, max ~1.37
            size-scale (+ 0.5 (* 0.5 (js/Math.sqrt (/ level 3.0))))]
        ;; diffuse color
        (.copy tmp-color color-off)
        (.lerp tmp-color color-on v)
        (.copy (.-color mat) tmp-color)
        ;; emissive
        (.copy tmp-color emissive-off)
        (.lerp tmp-color emissive-on v)
        (.copy (.-emissive mat) tmp-color)
        ;; emissive intensity
        (set! (.-emissiveIntensity mat) (+ 0.5 (* 0.5 v)))
        ;; scale: area-proportional to level
        (.set (.-scale mesh) size-scale size-scale size-scale)
        (set! (.-needsUpdate mat) true)))))

(defn- animate-frame! []
  (when-let [{:keys [tick-start] :as st} @scene-state]
    (when tick-start
      (let [elapsed  (- (js/performance.now) tick-start)
            progress (min 1.0 (/ elapsed (* @tick-ms anim-ratio)))]
        (animate-edges! st progress)
        (animate-nodes! st progress)
        ;; when animation completes, clear tick-start and start next if queued
        (when (>= progress 1.0)
          (swap! scene-state assoc :tick-start nil)
          (when @pending-state
            (apply-pending-state!)))))))

;; ---- setup ----

(defn setup! [container-el]
  (let [w      (.-clientWidth container-el)
        h      (.-clientHeight container-el)
        scene  (THREE/Scene.)
        camera (THREE/PerspectiveCamera. 60 (/ w h) 0.1 200)
        renderer (THREE/WebGLRenderer. #js {:antialias true :alpha true})
        mouse-state (atom {:dragging false :prev-x 0 :prev-y 0
                           :theta 0.8 :phi 1.2 :radius 8})]

    (.setSize renderer w h)
    (.setPixelRatio renderer js/window.devicePixelRatio)
    (set! (.. renderer -domElement -style -borderRadius) "8px")
    (.appendChild container-el (.-domElement renderer))

    (set! (.-background scene) (THREE/Color. 0x0a0e17))

    ;; lighting
    (let [ambient (THREE/AmbientLight. 0x334466 0.6)
          point1  (THREE/PointLight. 0x00ffcc 1.5 50)
          point2  (THREE/PointLight. 0xff6644 0.8 50)]
      (.set (.-position point1) 5 8 5)
      (.set (.-position point2) -5 6 -5)
      (.add scene ambient point1 point2))


    (.set (.-position camera) 0 5 8)
    (.lookAt camera 0 0 0)

    ;; mouse orbit controls
    (let [dom (.-domElement renderer)]
      (.addEventListener dom "mousedown"
        (fn [e]
          (swap! mouse-state assoc :dragging true
                 :prev-x (.-clientX e) :prev-y (.-clientY e))))
      (.addEventListener dom "mousemove"
        (fn [e]
          (when (:dragging @mouse-state)
            (let [dx (- (.-clientX e) (:prev-x @mouse-state))
                  dy (- (.-clientY e) (:prev-y @mouse-state))]
              (swap! mouse-state
                     (fn [s]
                       (-> s
                           (update :theta - (* dx 0.005))
                           (update :phi #(max 0.1 (min js/Math.PI (- % (* dy 0.005)))))
                           (assoc :prev-x (.-clientX e) :prev-y (.-clientY e)))))))))
      (.addEventListener dom "mouseup"
        (fn [_] (swap! mouse-state assoc :dragging false)))
      (.addEventListener dom "mouseleave"
        (fn [_] (swap! mouse-state assoc :dragging false)))
      (.addEventListener dom "wheel"
        (fn [e]
          (.preventDefault e)
          (swap! mouse-state update :radius
                 #(max 2 (min 60 (+ % (* (.-deltaY e) 0.01))))))))

    ;; groups
    (let [node-group (THREE/Group.)
          edge-group (THREE/Group.)]
      (.add scene node-group edge-group)

      ;; render loop
      (let [animate (fn animate []
                      (js/requestAnimationFrame animate)
                      (let [{:keys [theta phi radius]} @mouse-state
                            x (* radius (js/Math.sin phi) (js/Math.cos theta))
                            y (* radius (js/Math.cos phi))
                            z (* radius (js/Math.sin phi) (js/Math.sin theta))]
                        (.set (.-position camera) x y z)
                        (.lookAt camera 0 0 0))
                      (animate-frame!)
                      (.render renderer scene camera))]
        (animate))

      ;; resize
      (.addEventListener js/window "resize"
        (fn []
          (let [w (.-clientWidth container-el)
                h (.-clientHeight container-el)]
            (set! (.-aspect camera) (/ w h))
            (.updateProjectionMatrix camera)
            (.setSize renderer w h))))

      (reset! scene-state
              {:scene       scene
               :camera      camera
               :renderer    renderer
               :node-group  node-group
               :edge-group  edge-group
               :mouse-state mouse-state}))))

(defn dispose! []
  (when-let [{:keys [renderer]} @scene-state]
    (.dispose renderer)
    (reset! scene-state nil)))
