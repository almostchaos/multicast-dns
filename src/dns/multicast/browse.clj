(ns dns.multicast.browse
  (:require
    [socket.io.udp :refer [socket]]
    [dns.multicast.message :refer [encode-srv-query-message]]
    [clj-commons.byte-streams :refer [to-string print-bytes]]
    [taoensso.timbre :refer [debug info warn error]]))

(defmacro on-term-signal [& handler]
  `(.addShutdownHook (Runtime/getRuntime)
                     (Thread. (fn []
                                (debug "sigterm captured")
                                ~@handler))))

(def mdns-port 5353)
(def multicast-host "224.0.0.251")

(defn browse [protocol type & subtypes]
  (debug "starting browser ...")
  (debug "listening for mdns response ...")

  (let [message-bytes (encode-srv-query-message protocol type subtypes)

        {send :send close :close}
        (socket multicast-host mdns-port
                (fn [host port message]
                  (println "received [" host ":" port "] ------------")
                  (print-bytes message)))]

    (on-term-signal
      (info "stopping browser...")
      (close)
      (shutdown-agents)
      (info "stopped browser"))

    (future
      (Thread/sleep 5000)
      (debug "sending mdns request")
      (send multicast-host mdns-port message-bytes)
      (debug "sent mdns request"))))

(defn -main [& args]
  (browse "udp" "sleep-proxy"))