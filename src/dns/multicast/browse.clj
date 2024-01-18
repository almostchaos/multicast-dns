(ns dns.multicast.browse
  (:require
    [clojure.core.async :as async :refer [<!! >!!]]
    [socket.io.udp :refer [socket]]
    [tick.core :as tick]
    [dns.encoding :refer [decode-message, op-code:inverse-query]]
    [dns.message :refer [srv-query]]
    [clj-commons.byte-streams :refer [to-string print-bytes]]
    [taoensso.timbre :refer [debug info warn error]]))

(def mdns-port 5353)
(def multicast-host "224.0.0.251")

(defn- result-sequence [messages wait end-callback]
  (lazy-seq
    (if (tick/> (tick/instant) wait)
      (let
        [last-result (cons (<!! messages) nil)]
        (end-callback)
        last-result)
      (cons (<!! messages) (result-sequence messages wait end-callback)))))

(defn browse [protocol type & subtypes]
  (debug "starting browser ...")
  (debug "listening for mdns response ...")

  (let [message-bytes (srv-query protocol type subtypes)
        messages (async/chan)
        queue-message (fn [& parameters]
                        (>!! messages parameters))
        {send :send close :close} (socket multicast-host mdns-port queue-message)]

    (debug "sending mdns request")
    (send multicast-host mdns-port message-bytes)
    (debug "sent mdns request")

    (let [listen-until (tick/>> (tick/instant) (tick/new-duration 120 :seconds))]
      (->> (result-sequence messages listen-until (fn []
                                                    (close)
                                                    (async/close! messages)))
           (map (fn [[host port message]]
                  [host port (decode-message message)]))
           (filter (fn [[host port message]]
                     true))))))

(defn -main [& args]
  (run!
    (fn [[host port message]]
      (println "received [" host ":" port "] ------------")
      (clojure.pprint/pprint message))
    (browse "tcp" "smb"))
  (shutdown-agents))