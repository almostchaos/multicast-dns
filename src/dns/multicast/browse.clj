(ns dns.multicast.browse
  (:require
    [clj-commons.byte-streams :refer [to-string print-bytes]]
    [clojure.core.async :as async :refer [<!! >!!]]
    [clojure.string :as string]
    [dns.encoding :refer :all]
    [dns.message :refer [srv-query]]
    [socket.io.udp :refer [socket]]
    [taoensso.timbre :refer [debug]]))

(def mdns-port 5353)
(def multicast-host "224.0.0.251")

(defn- resource-type-matcher [type] (fn [section] (= type (:TYPE section))))
(def match-ptr (resource-type-matcher type:PTR))
(def match-srv (resource-type-matcher type:SRV))
(def match-a (resource-type-matcher type:A))

(defn- result-sequence [messages end-callback]
  (lazy-seq
    (let [message (<!! messages)]
      (if (nil? message)
        (do
          (end-callback)
          message)
        (cons message (result-sequence messages end-callback))))))


(defn browse [service-path]
  (debug "starting browser ...")
  (debug "listening for mdns response ...")

  (let [message-bytes (srv-query service-path)
        messages (async/timeout 20000)
        queue-message (fn [& parameters]
                        (>!! messages parameters))
        {send :send close :close} (socket multicast-host mdns-port queue-message)]

    (debug "sending mdns request")
    (send multicast-host mdns-port message-bytes)
    (debug "sent mdns request")

    (->> (result-sequence messages close)
         (map (fn [[host port message]] (decode-message message)))
         (filter (fn [message]
                   (let [header (first message)
                         body (rest message)
                         answer-count (:ANCOUNT header)]
                     (and
                       (> answer-count 0)
                       (some match-ptr body)
                       (= service-path (-> (filter match-ptr message) first :NAME))))))
         (map (fn [message]
                (let [ptr (first (filter match-ptr message))
                      a (first (filter match-a message))
                      result [(:NAME ptr) (:RDATA ptr)]]
                  (if (nil? a) result (conj result (:NAME a)))))))))

(defn -main [& args]
  (run!
    (fn [message]
      (println "------------")
      (clojure.pprint/pprint message))
    (browse "_airplay._tcp.local"))
  (shutdown-agents))