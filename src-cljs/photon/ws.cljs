(ns photon.ws
  (:require [reagent.core :as r]))

(defonce state (r/atom {:connected false
                        :running   false
                        :network   nil}))

(defonce socket (atom nil))

(defn send! [msg]
  (when-let [ws @socket]
    (when (= 1 (.-readyState ws))
      (.send ws (js/JSON.stringify (clj->js msg))))))

(defn connect! []
  (let [proto (if (= "https:" js/location.protocol) "wss:" "ws:")
        url   (str proto "//" js/location.host "/ws")
        ws    (js/WebSocket. url)]
    (set! (.-onopen ws)
          (fn [_]
            (swap! state assoc :connected true)
            (reset! socket ws)))
    (set! (.-onclose ws)
          (fn [_]
            (swap! state assoc :connected false)
            (reset! socket nil)
            ;; reconnect after 2s
            (js/setTimeout connect! 2000)))
    (set! (.-onmessage ws)
          (fn [evt]
            (let [msg (js->clj (js/JSON.parse (.-data evt)) :keywordize-keys true)]
              (case (:type msg)
                "state"  (swap! state assoc :network (:data msg))
                "status" (swap! state assoc :running (:running msg))
                nil))))))
