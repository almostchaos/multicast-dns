(ns socket.io.udp-test
  (:require
    [clojure.test :refer :all]
    [socket.io.udp :refer :all]
    [clj-commons.byte-streams :refer [to-string]])
  (:import
    (java.net DatagramSocket)))

(defn- free-port []
  (with-open [s (DatagramSocket. 0)]
    (.getLocalPort s)))

(deftest max-payload-constant-test
  (testing "max-payload is standard DNS over UDP payload size"
    (is (= 508 max-payload))))

(deftest socket-creation-and-lifecycle-test
  (testing "socket returns send and close functions"
    (let [{:keys [send close] :as s} (socket "127.0.0.1" 0 nil)]
      (is (map? s))
      (is (fn? send))
      (is (fn? close))
      ;; Closing socket
      (is (nil? (close)))
      ;; Subsequent close does not throw
      (is (nil? (close)))
      ;; Send after close is safe and does nothing
      (is (nil? (send "127.0.0.1" 12345 (byte-array [1 2 3])))))))

(deftest nil-parameters-test
  (testing "socket creation when receiver is nil does not start listener"
    (let [{:keys [send close]} (socket "127.0.0.1" 0 nil)]
      (is (fn? send))
      (is (fn? close))
      (close)))

  (testing "socket creation when address is nil does not start listener"
    (let [{:keys [send close]} (socket nil 0 nil)]
      (is (fn? send))
      (is (fn? close))
      (close))))

(deftest unicast-send-and-receive-test
  (testing "sending and receiving byte array message over unicast UDP"
    (let [port (free-port)
          received (promise)
          receiver (fn [host port data]
                     (deliver received {:host host :port port :data (vec data)}))
          {close-receiver :close} (socket "127.0.0.1" port receiver)
          {send-msg :send close-sender :close} (socket "127.0.0.1" 0 nil)
          payload (byte-array [10 20 30 40 50])]
      (try
        (send-msg "127.0.0.1" port payload)
        (let [result (deref received 2000 :timeout)]
          (is (not= :timeout result))
          (is (= [10 20 30 40 50] (:data result)))
          (is (string? (:host result)))
          (is (pos-int? (:port result))))
        (finally
          (close-receiver)
          (close-sender))))))

(deftest string-message-test
  (testing "sending string message via to-byte-array conversion"
    (let [port (free-port)
          received (promise)
          receiver (fn [_ _ data]
                     (deliver received (to-string data)))
          {close-receiver :close} (socket "127.0.0.1" port receiver)
          {send-msg :send close-sender :close} (socket "127.0.0.1" 0 nil)
          msg "hello udp world"]
      (try
        (send-msg "127.0.0.1" port msg)
        (let [result (deref received 2000 :timeout)]
          (is (= msg result)))
        (finally
          (close-receiver)
          (close-sender))))))

(deftest payload-size-test
  (testing "payload length is preserved without padding"
    (let [port (free-port)
          received (promise)
          receiver (fn [_ _ data]
                     (deliver received (count data)))
          {close-receiver :close} (socket "127.0.0.1" port receiver)
          {send-msg :send close-sender :close} (socket "127.0.0.1" 0 nil)]
      (try
        ;; Send a single byte
        (send-msg "127.0.0.1" port (byte-array [1]))
        (is (= 1 (deref received 2000 :timeout)))
        (finally
          (close-receiver)
          (close-sender)))))

  (testing "payload at max-payload boundary (508 bytes)"
    (let [port (free-port)
          received (promise)
          receiver (fn [_ _ data]
                     (deliver received (vec data)))
          {close-receiver :close} (socket "127.0.0.1" port receiver)
          {send-msg :send close-sender :close} (socket "127.0.0.1" 0 nil)
          payload (byte-array (map #(byte (mod % 128)) (range max-payload)))]
      (try
        (send-msg "127.0.0.1" port payload)
        (let [result (deref received 2000 :timeout)]
          (is (= (vec payload) result))
          (is (= max-payload (count result))))
        (finally
          (close-receiver)
          (close-sender))))))

(deftest multiple-packets-test
  (testing "receiving multiple packets sequentially on the same socket"
    (let [port (free-port)
          packet-count 5
          received (atom [])
          done (promise)
          receiver (fn [_ _ data]
                     (let [all (swap! received conj (vec data))]
                       (when (= (count all) packet-count)
                         (deliver done all))))
          {close-receiver :close} (socket "127.0.0.1" port receiver)
          {send-msg :send close-sender :close} (socket "127.0.0.1" 0 nil)]
      (try
        (dotimes [i packet-count]
          (send-msg "127.0.0.1" port (byte-array [i])))
        (let [result (deref done 3000 :timeout)]
          (is (not= :timeout result))
          (is (= [[0] [1] [2] [3] [4]] result)))
        (finally
          (close-receiver)
          (close-sender))))))

(deftest multicast-socket-test
  (testing "multicast socket lifecycle and transmission"
    (let [port (free-port)
          multicast-addr "224.0.0.251"
          received (promise)
          receiver (fn [host _ data]
                     (deliver received {:host host :data (vec data)}))
          {receiver-send :send close-receiver :close} (socket "0.0.0.0" port receiver :multicast multicast-addr)
          {sender-send :send close-sender :close} (socket "0.0.0.0" 0 nil)
          payload (byte-array [99 98 97])]
      (try
        (is (fn? receiver-send))
        (is (fn? close-receiver))
        (sender-send multicast-addr port payload)
        (let [result (deref received 2000 :timeout)]
          (is (not= :timeout result))
          (is (= [99 98 97] (:data result))))
        (finally
          (close-receiver)
          (close-sender))))))
