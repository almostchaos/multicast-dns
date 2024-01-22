(ns dns.unicast.query
  (:require
    [clojure.core.async :as async :refer [<!! >!!]]
    [dns.encoding :refer :all]
    [dns.message :refer :all]
    [socket.io.udp :refer [socket]]
    [taoensso.timbre :refer [debug]]))

(defn name->ip
  ([name]
   (name->ip name "8.8.8.8"))
  ([name dns]
   (debug "starting client ...")
   (debug "listening for dns response ...")

   (let [message-bytes (a-query name)
         messages (async/timeout 500)
         {send :send close-socket :close}
         ;;use all network interfaces and pick a random port
         (socket "0.0.0.0" 0 (fn [& parameters] (>!! messages parameters)))]

     (debug "sending dns request")
     (send dns 53 message-bytes)
     (debug "sent dns request")

     (let [[host port message] (<!! messages)]
       (close-socket)
       (->
         (->>
           (decode-message message)
           (filter
             (fn [section]
               (= type:A (:TYPE section)))))
         (first)
         (:RDATA))))))


(defn -main [& args]
  (println (name->ip "wikipedia.org"))

  (shutdown-agents))
