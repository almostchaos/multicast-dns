(ns socket.io.udp
  (:require
    [clj-commons.byte-streams :refer [to-byte-array]])
  (:import
    (java.net DatagramPacket DatagramSocket InetAddress InetSocketAddress MulticastSocket NetworkInterface)))

(def max-payload 508)

(defn socket
  "Opens a unicast or multicast UDP socket and begins listening asynchronously.

  Arguments:
  - address   : Host/IP string to bind (e.g. \"0.0.0.0\").
  - port      : Local port number to bind.
  - receiver  : Callback fn (fn [host port data-bytes]) invoked for each incoming packet.
  - control   : Setup fn (fn [send close]) called with functions to transmit datagrams
                and shut down the socket.
  - multicast : (Optional keyword arg) Multicast group IP to join (e.g. \"224.0.0.251\")."

  [address port receiver control & {multicast :multicast}]
  (let [bind-address (InetAddress/getByName address)
        [inet-socket close] (if multicast
                              (let [multicast-address (InetAddress/getByName multicast)
                                    socket-address (new InetSocketAddress multicast-address port)
                                    multicast-socket (new MulticastSocket port)
                                    interface (NetworkInterface/getByInetAddress bind-address)
                                    close-socket (fn []
                                                   (.leaveGroup multicast-socket socket-address interface)
                                                   (.close multicast-socket))]
                                (.joinGroup multicast-socket socket-address interface)
                                [multicast-socket close-socket])

                              (let [socket-address (new InetSocketAddress bind-address port)
                                    datagram-socket (new DatagramSocket socket-address)
                                    close-socket (fn [] (.close datagram-socket))]
                                [datagram-socket close-socket]))
        send (fn [destination-address destination-port message]
               (when-not (.isClosed inet-socket)
                 (let [address (InetAddress/getByName destination-address)
                       data (to-byte-array message)
                       data-length (alength data)]
                   (.send inet-socket
                          (new DatagramPacket data data-length address destination-port)))))]

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

    (control send close)))
