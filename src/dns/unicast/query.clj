(ns dns.unicast.query
  (:require
    [clojure.core.async :as async :refer [<!! >!!]]
    [dns.encoding :refer :all]
    [dns.message :refer :all]
    [socket.io.udp :refer [socket]]
    [taoensso.timbre :refer [debug]]))

(defn name->ip [dns name]
  (debug "starting client ...")
  (debug "listening for dns response ...")

  (let [message-bytes (time (a-query name))
        messages (async/timeout 500)
        {send :send close-socket :close}
        ;;use all network interfaces and pick a random port
        (socket "0.0.0.0" 0 (fn [& parameters] (>!! messages parameters)))]

    (debug "sending dns request")
    (send dns 53 message-bytes)
    (debug "sent dns request")

    (let [result (<!! messages)]
      (close-socket)
      result)))


(defn -main [& args]
  (let [[host port message] (time (name->ip "8.8.8.8" "wikipedia.org"))]
    (println "received [" host ":" port "] ------------")
    (clojure.pprint/pprint (decode-message message)))

  (shutdown-agents))
