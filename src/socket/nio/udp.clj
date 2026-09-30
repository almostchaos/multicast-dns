(ns socket.nio.udp
  (:require
    [clj-commons.byte-streams :refer [to-byte-array]]
    [socket.io.udp :refer [max-payload]])
  (:import
    (java.net Inet4Address InetAddress InetSocketAddress NetworkInterface
              StandardProtocolFamily StandardSocketOptions)
    (java.nio ByteBuffer)
    (java.nio.channels AsynchronousCloseException ClosedChannelException DatagramChannel)))

(defn- resolve-multicast-network-interface
  "Resolves the NetworkInterface to use for multicast join/send operations based
  on the requested bind address."
  [^InetAddress bind-address]
  (or
    (when (and bind-address (not (.isAnyLocalAddress bind-address)))
      (NetworkInterface/getByInetAddress bind-address))
    (some (fn [^NetworkInterface ni]
            (when (and (.isUp ni)
                       (.supportsMulticast ni)
                       (some (fn [a] (instance? Inet4Address a))
                             (enumeration-seq (.getInetAddresses ni))))
              ni))
          (enumeration-seq (NetworkInterface/getNetworkInterfaces)))
    (some (fn [^NetworkInterface ni]
            (when (and (.isUp ni) (.supportsMulticast ni))
              ni))
          (enumeration-seq (NetworkInterface/getNetworkInterfaces)))
    (NetworkInterface/getByInetAddress (InetAddress/getByName "127.0.0.1"))))

(defn socket
  "Opens a unicast or multicast UDP socket using Java NIO DatagramChannel and
  begins listening asynchronously.

  Arguments:
  - address   : Host/IP string to bind (e.g. \"0.0.0.0\").
  - port      : Local port number to bind.
  - receiver  : Callback fn (fn [host port data-bytes]) invoked for each incoming packet.
  - control   : Setup fn (fn [send close]) called with functions to transmit datagrams
                and shut down the socket.
  - multicast : (Optional keyword arg) Multicast group IP to join (e.g. \"224.0.0.251\")."

  [address port receiver control & {multicast :multicast}]
  (let [bind-address (when address (InetAddress/getByName address))
        [channel close] (if multicast
                          (let [multicast-address (InetAddress/getByName multicast)
                                interface (resolve-multicast-network-interface bind-address)
                                ch (DatagramChannel/open StandardProtocolFamily/INET)]
                            (when (.contains (.supportedOptions ch) StandardSocketOptions/SO_REUSEADDR)
                              (.setOption ch StandardSocketOptions/SO_REUSEADDR (Boolean/valueOf true)))
                            (when (.contains (.supportedOptions ch) StandardSocketOptions/SO_REUSEPORT)
                              (.setOption ch StandardSocketOptions/SO_REUSEPORT (Boolean/valueOf true)))
                            (when interface
                              (when (.contains (.supportedOptions ch) StandardSocketOptions/IP_MULTICAST_IF)
                                (.setOption ch StandardSocketOptions/IP_MULTICAST_IF interface)))
                            (.bind ch (InetSocketAddress. port))
                            (let [membership-key (when interface (.join ch multicast-address interface))
                                  close-channel (fn []
                                                  (when membership-key
                                                    (try (.drop membership-key) (catch Exception _)))
                                                  (when (.isOpen ch)
                                                    (try (.close ch) (catch Exception _))))]
                              [ch close-channel]))

                          (let [socket-address (if bind-address
                                                 (InetSocketAddress. bind-address (int port))
                                                 (InetSocketAddress. (int port)))
                                ch (DatagramChannel/open StandardProtocolFamily/INET)]
                            (when (.contains (.supportedOptions ch) StandardSocketOptions/SO_REUSEADDR)
                              (.setOption ch StandardSocketOptions/SO_REUSEADDR (Boolean/valueOf true)))
                            (.bind ch socket-address)
                            (let [close-channel (fn []
                                                  (when (.isOpen ch)
                                                    (try (.close ch) (catch Exception _))))]
                              [ch close-channel])))
        send (fn [destination-address destination-port message]
               (when (.isOpen channel)
                 (let [dest-address (InetAddress/getByName destination-address)
                       target-address (InetSocketAddress. dest-address (int destination-port))
                       data (to-byte-array message)
                       buffer (ByteBuffer/wrap data)]
                   (.send channel buffer target-address))))]

    (when-not (or (nil? address) (nil? receiver))
      (future
        (let [buffer (ByteBuffer/allocate max-payload)]
          (try
            (loop []
              (.clear buffer)
              (let [^InetSocketAddress sender (.receive channel buffer)]
                (when (and sender (.isOpen channel))
                  (.flip buffer)
                  (let [len (.remaining buffer)
                        data (byte-array len)]
                    (.get buffer data)
                    (receiver
                      (.getHostName (.getAddress sender))
                      (.getPort sender)
                      data))
                  (when (.isOpen channel)
                    (recur)))))
            (catch AsynchronousCloseException _)
            (catch ClosedChannelException _)
            (catch Exception e
              (when (.isOpen channel)
                (throw e)))))))

    (control send close)))
