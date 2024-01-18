(ns dns.multicast.browse
  (:require
    [clojure.core.async :as async :refer [<!! >!!]]
    [socket.io.udp :refer [socket]]
    [tick.core :as tick]
    [dns.encoding :refer :all]
    [dns.message :refer [srv-query]]
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

(defn- match-ptr [section] (= resource-type:PTR (:TYPE section)))
(defn- match-a [section] (= resource-type:A (:TYPE section)))

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
           (map (fn [[host port message]] (decode-message message)))
           (filter (fn [message]
                     (let [header (first message)
                           body (rest message)
                           answer-count (:ANCOUNT header)]
                       (and
                         (> answer-count 0)
                         (some match-a body)
                         (some match-ptr body)))))
           (map (fn [message]
                  (let [ptr (first (filter match-ptr message))
                        a (first (filter match-a message))]
                    [(:RDATA ptr) (:NAME a) (:RDATA a)])))))))

(defn -main [& args]
  (run!
    (fn [message]
      (println "------------")
      (clojure.pprint/pprint message))
    (browse "tcp" "smb"))
  (shutdown-agents))