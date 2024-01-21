(ns dns.unicast.query
  (:require
    [clojure.core.async :as async :refer [<!! >!!]]
    [dns.encoding :refer :all]
    [dns.message :refer :all]
    [socket.io.udp :refer [socket]]
    [taoensso.timbre :refer [debug]]))

(def mdns-port 53)
(def multicast-host "193.231.252.1")

(defn query
  [name]
  (debug "starting browser ...")
  (debug "listening for mdns response ...")

  (let [message-bytes (time (a-query name))
        messages (async/timeout 500)
        {send :send close :close}
        (socket "0.0.0.0" 10001 (fn [& parameters] (>!! messages parameters)))]

    (debug "sending mdns request")
    (send multicast-host mdns-port message-bytes)
    (debug "sent mdns request")

    (let [result (<!! messages)]
      (close)
      result)))


(defn -main [& args]
  (let [[host port message] (query "mail.yahoo.com")]
    (println "received [" host ":" port "] ------------")
    (clojure.pprint/pprint (decode-message message)))

  (shutdown-agents))
