(ns photon.core
  (:require [photon.server :as server]))

(defn -main [& _args]
  (server/start! {:port 8080}))
