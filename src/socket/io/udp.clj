(ns socket.io.udp
  (:require
    [clj-commons.byte-streams :refer [to-byte-array]])
  (:import
    (java.net DatagramPacket DatagramSocket InetAddress InetSocketAddress MulticastSocket NetworkInterface)))

(def max-payload 508)

(defn socket [address port receiver]
  "Naive implementation of UDP sockets."

  (let [inet-address (InetAddress/getByName address)
        socket-address (new InetSocketAddress inet-address port)
        inet-socket (if (.isMulticastAddress inet-address)
                      (let [multicast-socket (new MulticastSocket port)
                            network-interface (NetworkInterface/getByInetAddress inet-address)]
                        (.joinGroup multicast-socket socket-address network-interface)
                        multicast-socket)

                      (new DatagramSocket socket-address))]

    (when-not (or (nil? address) (nil? receiver))
      (future
        (loop []
          (let [data (byte-array max-payload)
                packet (new DatagramPacket data max-payload)]
            (.receive inet-socket packet)
            (receiver
              (.getHostName (.getAddress packet))
              (.getPort packet)
              (byte-array (take (.getLength packet) data))))
          (when-not (.isClosed inet-socket)
            (recur)))))

    {:send  (fn [destination-address destination-port message]
              (when-not (.isClosed inet-socket)
                (let [address (InetAddress/getByName destination-address)
                      data (to-byte-array message)
                      data-length (alength data)]
                  (.send inet-socket
                         (new DatagramPacket data data-length address destination-port)))))

     :close (fn []
              (.close inet-socket))}))