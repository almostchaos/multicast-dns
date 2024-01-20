(ns dns.multicast.browse
  (:require
    [clojure.core.async :as async :refer [<!! >!!]]
    [dns.encoding :refer :all]
    [dns.message :refer [srv-query]]
    [socket.io.udp :refer [socket]]
    [taoensso.timbre :refer [debug]]))

(def mdns-port 5353)
(def multicast-host "224.0.0.251")

(defn- resource-type-matcher [type] (fn [section] (= type (:TYPE section))))
(def match-ptr (resource-type-matcher resource-type:PTR))
(def match-a (resource-type-matcher resource-type:A))

(defn- result-sequence [messages end-callback]
  (lazy-seq
    (let [message (<!! messages)]
      (if (nil? message)
        (do
          (end-callback)
          message)
        (cons message (result-sequence messages end-callback))))))


(defn browse [protocol type & subtypes]
  (debug "starting browser ...")
  (debug "listening for mdns response ...")

  (let [message-bytes (srv-query protocol type subtypes)
        messages (async/timeout 120000)
        queue-message (fn [& parameters]
                        (>!! messages parameters))
        {send :send close :close} (socket multicast-host mdns-port queue-message)]

    (debug "sending mdns request")
    (send multicast-host mdns-port message-bytes)
    (debug "sent mdns request")

    (->> (result-sequence messages (fn [] (close) (async/close! messages)))
         (map (fn [[host port message]] (decode-message message)))
         (filter (fn [message]
                   (let [header (first message)
                         body (rest message)
                         answer-count (:ANCOUNT header)]
                     (and
                       (> answer-count 0)
                       (some match-ptr body)))))
         (map (fn [message]
                (let [ptr (first (filter match-ptr message))]
                  [(:NAME ptr) (:RDATA ptr)]))))))

(defn -main [& args]
  (run!
    (fn [message]
      (println "------------")
      (clojure.pprint/pprint message))
    (browse "tcp" "smb"))
  (shutdown-agents))