(ns socket.io.udp
  (:require
    [clj-commons.byte-streams :refer [to-byte-array]])
  (:import
    (java.net DatagramPacket DatagramSocket InetAddress InetSocketAddress MulticastSocket NetworkInterface)))

(def max-payload 508)

(defn socket [address port receiver & {multicast :multicast}]
  "Naive implementation of UDP sockets."

  (let [bind-address (InetAddress/getByName address)
        inet-socket (if multicast
                      (let [multicast-address (InetAddress/getByName multicast)
                            socket-address (new InetSocketAddress multicast-address port)
                            multicast-socket (new MulticastSocket port)
                            interface (NetworkInterface/getByInetAddress bind-address)]
                        (.joinGroup multicast-socket socket-address interface)
                        multicast-socket)

                      (let [socket-address (new InetSocketAddress bind-address port)]
                        (new DatagramSocket socket-address)))]

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