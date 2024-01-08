(ns socket.io.tcp
  (:require
    [manifold.stream :refer [stream put! take! close! closed?]]
    [clj-commons.byte-streams :refer [to-string to-byte-array print-bytes]])
  (:import
    (java.net DatagramPacket DatagramSocket InetAddress)))

(def max-payload 508)

(defn socket [address port options]
  "Naive implementation of UDP sockets."
  (let [source-address (InetAddress/getByName address)
        datagram-socket (new DatagramSocket port source-address)
        in (stream 10)
        out (stream 10)]

    {:send    (fn [destination-address destination-port]
                (let [address (InetAddress/getByName destination-address)]
                  (future
                    (loop [data (to-byte-array @(take! in))]
                      (.send datagram-socket
                             (new DatagramPacket data (alength data) address destination-port))
                      (if (.isClosed datagram-socket)
                        (close! in)
                        (when-not (closed? in)
                          (recur (to-byte-array @(take! in)))))))
                  in))

     :receive (fn []
                (future
                  (loop [data (byte-array max-payload)]
                    (let [packet (new DatagramPacket data (alength data))]
                      (.receive datagram-socket packet)
                      @(put! out (byte-array (take (.getLength packet) data)))
                      (if (.isClosed datagram-socket)
                        (close! out)
                        (when-not (closed? out)
                          (recur (byte-array max-payload)))))))
                out)

     :close   (fn []
                (.close datagram-socket)
                (close! in)
                (close! out))}))

(defn -main [& args]
  (let [{:keys [send receive close]} (socket "localhost" 10001 {})
        in (send "localhost" 10002)
        out (receive)]

    (put! in "ping")
    (println "received ->" (to-string @(take! out)))
    (close)
    (shutdown-agents)))