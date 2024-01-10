(ns dns.multicast.browse
  (:require
    [clojure.core.async :as async :refer [<!! >!! <! >!]]
    [socket.io.udp :refer [socket]]
    [dns.multicast.message :refer [encode-srv-query-message]]
    [clj-commons.byte-streams :refer [to-string print-bytes]]
    [taoensso.timbre :refer [debug info warn error]]))

(defmacro on-term-signal [& handler]
  `(.addShutdownHook
     (Runtime/getRuntime)
     (Thread. ^Runnable
              (fn []
                (debug "sigterm captured")
                ~@handler))))

(def mdns-port 5353)
(def multicast-host "224.0.0.251")

(defn results [messages]
  (lazy-seq (cons (<!! messages) (results messages))))

(defn browse [protocol type & subtypes]
  (debug "starting browser ...")
  (debug "listening for mdns response ...")

  (let [message-bytes (encode-srv-query-message protocol type subtypes)
        received-messages (async/chan)
        {send :send close :close}
        (socket multicast-host mdns-port
                (fn [& parameters] (>!! received-messages parameters)))]

    (future
      (Thread/sleep 5000)
      (debug "sending mdns request")
      (send multicast-host mdns-port message-bytes)
      (debug "sent mdns request"))

    (results received-messages)))

(defn -main [& args]
  (run!
    (fn [[host port message]]
      (println "received [" host ":" port "] ------------")
      (print-bytes message))
    (browse "udp" "sleep-proxy"))
  (shutdown-agents))