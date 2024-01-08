(ns socket.io.udp
  (:require
    [clj-commons.byte-streams :refer [to-string to-byte-array print-bytes]])
  (:import
    (java.net DatagramPacket DatagramSocket InetAddress InetSocketAddress MulticastSocket)))

(def max-payload 508)

(defn socket [address port receiver options]
  "Naive implementation of UDP sockets."

  (let [datagram-socket
        (let [inet-address (InetAddress/getByName address)]
          (if (.isMulticastAddress inet-address)
            (let [multicast-socket (new MulticastSocket port)]
              (.joinGroup multicast-socket inet-address)
              multicast-socket)
            (new DatagramSocket inet-address port)))]

    (when-not (or (nil? address) (nil? receiver))
      (future
        (loop []
          (let [data (byte-array max-payload)
                packet (new DatagramPacket data max-payload)]
            (.receive datagram-socket packet)
            (receiver
              (.getHostName (.getAddress packet))
              (.getPort packet)
              (byte-array (take (.getLength packet) data))))
          (when-not (.isClosed datagram-socket)
            (recur)))))

    {:send  (fn [destination-address destination-port message]
              (when-not (.isClosed datagram-socket)
                (let [address (InetAddress/getByName destination-address)
                      data (to-byte-array message)
                      data-length (alength data)]
                  (.send datagram-socket
                         (new DatagramPacket data data-length address destination-port)))))

     :close (fn []
              (.close datagram-socket))}))

(defn -main [& args]
  (let [receiver
        (fn [host port message]
          ;;(print-bytes message)
          (println "received [" host ":" port "] ->" (to-string (byte-array (take 50 message)))))
        {:keys [send close]} (socket "224.0.0.251" 5353 receiver {:reuse-address true})]

    (println "listening")
    (send "224.0.0.251" 5353 "ping")
    (future
      (Thread/sleep 30000)
      (close)
      (shutdown-agents))))