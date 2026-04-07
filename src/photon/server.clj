(ns photon.server
  (:require [org.httpkit.server :as hk]
            [compojure.core :refer [defroutes GET POST]]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [clojure.data.json :as json]
            [photon.simulation :as sim]))

;; ---- state ----

(defonce channels (atom #{}))          ;; connected ws clients
(defonce network  (atom nil))          ;; current network
(defonce running? (atom false))        ;; simulation running?
(defonce sim-future (atom nil))        ;; future for sim loop
(def tick-ms (atom 600))               ;; tick interval in ms

;; ---- broadcast ----

(defn broadcast! [msg]
  (let [payload (json/write-str msg)]
    (doseq [ch @channels]
      (try (hk/send! ch payload)
           (catch Exception _ nil)))))

(defn broadcast-state! []
  (when-let [net @network]
    (broadcast! {:type "state" :data (sim/network->client-data net)})))

;; ---- simulation loop ----

(defn stop-sim! []
  (reset! running? false)
  (when-let [f @sim-future]
    (future-cancel f)
    (reset! sim-future nil)))

(defn start-sim! []
  (when @network
    (reset! running? true)
    (reset! sim-future
            (future
              (try
                (while @running?
                  (swap! network sim/step)
                  (broadcast-state!)
                  (Thread/sleep (max 50 @tick-ms)))
                (catch Exception e
                  (when-not (instance? InterruptedException e)
                    (.printStackTrace e))))))))

;; ---- websocket handler ----

(defn ws-handler [req]
  (hk/as-channel req
    {:on-open    (fn [ch]
                   (swap! channels conj ch)
                   (when @network
                     (hk/send! ch (json/write-str
                                   {:type "state"
                                    :data (sim/network->client-data @network)})))
                   (hk/send! ch (json/write-str
                                 {:type "status"
                                  :running (boolean @running?)})))
     :on-close   (fn [ch _status]
                   (swap! channels disj ch))
     :on-receive (fn [ch msg]
                   (let [{:strs [action size density ms]} (json/read-str msg)]
                     (case action
                       "generate" (do
                                    (stop-sim!)
                                    (let [n   (or size 20)
                                          d   (or density 0.3)
                                          net (sim/generate-network n d)]
                                      (reset! network net)
                                      (broadcast-state!)
                                      (broadcast! {:type "status" :running false})))
                       "start"    (do
                                    (when-not @running? (start-sim!))
                                    (broadcast! {:type "status" :running true}))
                       "stop"     (do
                                    (stop-sim!)
                                    (broadcast! {:type "status" :running false}))
                       "step"     (when (and @network (not @running?))
                                    (swap! network sim/step)
                                    (broadcast-state!))
                       "set-tick" (when ms
                                    (reset! tick-ms (max 50 (min 5000 (long ms)))))
                       nil)))}))

;; ---- routes ----

(defn generate-page [_req]
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body    (slurp (clojure.java.io/resource "public/index.html"))})

(defroutes app-routes
  (GET "/generate" [] generate-page)
  (GET "/ws"       [] ws-handler)
  (route/resources "/")
  (route/not-found "<h1>Not found</h1>"))

(def app
  (-> app-routes
      (wrap-resource "public")
      wrap-content-type))

;; ---- start/stop server ----

(defonce server (atom nil))

(defn start! [& [{:keys [port] :or {port 8080}}]]
  (stop-sim!)
  (when @server (@server))
  (reset! server (hk/run-server app {:port port}))
  (println (str "Photon server running on http://localhost:" port "/generate")))

(defn stop! []
  (stop-sim!)
  (when @server
    (@server)
    (reset! server nil)))
