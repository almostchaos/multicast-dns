(ns socket.io.udp
  (:require
    [clj-commons.byte-streams :refer [to-string to-byte-array print-bytes]])
  (:import
    (java.net DatagramPacket DatagramSocket InetAddress InetSocketAddress MulticastSocket)))

(def max-payload 508)

(defn socket [address port receiver]
  "Naive implementation of UDP sockets."

  (let [inet-address (InetAddress/getByName address)
        multicast? (.isMulticastAddress inet-address)
        datagram-socket (if multicast?
                          (new MulticastSocket port)
                          (new DatagramSocket port inet-address))]

    (if multicast? (.joinGroup datagram-socket inet-address))

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
          (println "received [" host ":" port "] ------------")
          (print-bytes (byte-array (take 100 message))))

        {:keys [send close]} (socket "224.0.0.251" 5353 receiver)]

    (println "listening")
    (send "224.0.0.251" 5353 "ping")
    (future
      (Thread/sleep 40000)
      (close)
      (shutdown-agents))))