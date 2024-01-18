(ns dns.multicast.browse
  (:require
    [clojure.core.async :as async :refer [<!! >!! <! >!]]
    [socket.io.udp :refer [socket]]
    [tick.core :as tick]
    [dns.message :refer [encode-srv-query-message decode-message]]
    [clj-commons.byte-streams :refer [to-string print-bytes]]
    [taoensso.timbre :refer [debug info warn error]]))

(def mdns-port 5353)
(def multicast-host "224.0.0.251")

(defn- results [messages wait end-callback]
  (lazy-seq
    (if (tick/> (tick/instant) wait)
      (let
        [last-result (cons (<!! messages) nil)]
        (end-callback)
        last-result)
      (cons (<!! messages) (results messages wait end-callback)))))

(defn browse [protocol type & subtypes]
  (debug "starting browser ...")
  (debug "listening for mdns response ...")

  (let [message-bytes (encode-srv-query-message protocol type subtypes)
        received-messages (async/chan)
        {send :send close :close}
        (socket multicast-host
                mdns-port
                (fn [& parameters] (>!! received-messages parameters)))]

    (debug "sending mdns request")
    (send multicast-host mdns-port message-bytes)
    (debug "sent mdns request")

    (results received-messages
             (tick/>> (tick/instant) (tick/new-duration 360 :seconds))
             (fn []
               (close)
               (async/close! received-messages)))))

(defn -main [& args]
  (run!
    (fn [[host port message]]
      (println "received [" host ":" port "] ------------")
      (print-bytes message)
      (clojure.pprint/pprint (decode-message message)))
    (filter
      (fn [[host _ _]]
        true)                             ; (not= host "192.168.0.103")
      (browse "tcp" "smb")))
  (shutdown-agents))