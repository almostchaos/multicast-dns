(ns dns.unicast.query
  (:require
    [clojure.core.async :as async :refer [<!! >!!]]
    [dns.encoding :refer :all]
    [dns.message :refer :all]
    [socket.io.udp :refer [socket]]))

(defn name->ip
  ([name]
   (name->ip name "8.8.8.8"))
  ([name dns]
   (let [messages (async/timeout 500)
         receive (fn [_ _ message] (>!! messages message))
         {send :send close-socket :close}
         ;;use all network interfaces and pick a random port
         (socket "0.0.0.0" 0 receive)]

     (send dns 53 (a-query name))
     (let [message (<!! messages)]
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
