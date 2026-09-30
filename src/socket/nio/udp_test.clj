(ns socket.nio.udp-test
  (:require
    [clojure.test :refer :all]
    [socket.nio.udp :as nio]
    [socket.io.udp :as io]
    [clj-commons.byte-streams :refer [to-string]])
  (:import
    (java.net DatagramSocket)))

(defn- free-port []
  (with-open [s (DatagramSocket. 0)]
    (.getLocalPort s)))

(deftest nio-socket-creation-and-lifecycle-test
  (testing "socket provides send and close functions to control callback"
    (let [result (nio/socket "127.0.0.1" 0 nil
                             (fn [send close]
                               (is (fn? send))
                               (is (fn? close))
                               ;; Closing socket
                               (is (nil? (close)))
                               ;; Subsequent close does not throw
                               (is (nil? (close)))
                               ;; Send after close is safe and does nothing
                               (is (nil? (send "127.0.0.1" 12345 (byte-array [1 2 3]))))
                               :ok))]
      (is (= :ok result)))))

(deftest nio-nil-parameters-test
  (testing "socket creation when receiver is nil does not start listener"
    (nio/socket "127.0.0.1" 0 nil
                (fn [send close]
                  (is (fn? send))
                  (is (fn? close))
                  (close))))

  (testing "socket creation when address is nil does not start listener"
    (nio/socket nil 0 nil
                (fn [send close]
                  (is (fn? send))
                  (is (fn? close))
                  (close)))))

(deftest nio-unicast-send-and-receive-test
  (testing "sending and receiving byte array message over unicast UDP (nio/socket)"
    (let [port (free-port)
          received (promise)
          receiver (fn [host port data]
                     (deliver received {:host host :port port :data (vec data)}))
          payload (byte-array [10 20 30 40 50])]
      (nio/socket "127.0.0.1" port receiver
        (fn [_ close-receiver]
          (nio/socket "127.0.0.1" 0 nil
            (fn [send-msg close-sender]
              (try
                (send-msg "127.0.0.1" port payload)
                (let [result (deref received 2000 :timeout)]
                  (is (not= :timeout result))
                  (is (= [10 20 30 40 50] (:data result)))
                  (is (string? (:host result)))
                  (is (pos-int? (:port result))))
                (finally
                  (close-receiver)
                  (close-sender))))))))))

(deftest cross-socket-unicast-test
  (testing "sending from nio/socket and receiving on io/socket"
    (let [port (free-port)
          received (promise)
          receiver (fn [host port data]
                     (deliver received {:host host :port port :data (vec data)}))
          payload (byte-array [1 3 5 7 9])]
      (io/socket "127.0.0.1" port receiver
        (fn [_ close-receiver]
          (nio/socket "127.0.0.1" 0 nil
            (fn [send-msg close-sender]
              (try
                (send-msg "127.0.0.1" port payload)
                (let [result (deref received 2000 :timeout)]
                  (is (not= :timeout result))
                  (is (= [1 3 5 7 9] (:data result))))
                (finally
                  (close-receiver)
                  (close-sender)))))))))

  (testing "sending from io/socket and receiving on nio/socket"
    (let [port (free-port)
          received (promise)
          receiver (fn [host port data]
                     (deliver received {:host host :port port :data (vec data)}))
          payload (byte-array [2 4 6 8 10])]
      (nio/socket "127.0.0.1" port receiver
        (fn [_ close-receiver]
          (io/socket "127.0.0.1" 0 nil
            (fn [send-msg close-sender]
              (try
                (send-msg "127.0.0.1" port payload)
                (let [result (deref received 2000 :timeout)]
                  (is (not= :timeout result))
                  (is (= [2 4 6 8 10] (:data result))))
                (finally
                  (close-receiver)
                  (close-sender))))))))))

(deftest nio-string-message-test
  (testing "sending string message via to-byte-array conversion (nio/socket)"
    (let [port (free-port)
          received (promise)
          receiver (fn [_ _ data]
                     (deliver received (to-string data)))
          msg "hello nio udp world"]
      (nio/socket "127.0.0.1" port receiver
        (fn [_ close-receiver]
          (nio/socket "127.0.0.1" 0 nil
            (fn [send-msg close-sender]
              (try
                (send-msg "127.0.0.1" port msg)
                (let [result (deref received 2000 :timeout)]
                  (is (= msg result)))
                (finally
                  (close-receiver)
                  (close-sender))))))))))

(deftest nio-payload-size-test
  (testing "payload length is preserved without padding (nio/socket)"
    (let [port (free-port)
          received (promise)
          receiver (fn [_ _ data]
                     (deliver received (count data)))]
      (nio/socket "127.0.0.1" port receiver
        (fn [_ close-receiver]
          (nio/socket "127.0.0.1" 0 nil
            (fn [send-msg close-sender]
              (try
                (send-msg "127.0.0.1" port (byte-array [1]))
                (is (= 1 (deref received 2000 :timeout)))
                (finally
                  (close-receiver)
                  (close-sender)))))))))

  (testing "payload at max-payload boundary (508 bytes) (nio/socket)"
    (let [port (free-port)
          received (promise)
          receiver (fn [_ _ data]
                     (deliver received (vec data)))
          payload (byte-array (map #(byte (mod % 128)) (range io/max-payload)))]
      (nio/socket "127.0.0.1" port receiver
        (fn [_ close-receiver]
          (nio/socket "127.0.0.1" 0 nil
            (fn [send-msg close-sender]
              (try
                (send-msg "127.0.0.1" port payload)
                (let [result (deref received 2000 :timeout)]
                  (is (= (vec payload) result))
                  (is (= io/max-payload (count result))))
                (finally
                  (close-receiver)
                  (close-sender))))))))))

(deftest nio-multiple-packets-test
  (testing "receiving multiple packets sequentially on the same socket (nio/socket)"
    (let [port (free-port)
          packet-count 5
          received (atom [])
          done (promise)
          receiver (fn [_ _ data]
                     (let [all (swap! received conj (vec data))]
                       (when (= (count all) packet-count)
                         (deliver done all))))]
      (nio/socket "127.0.0.1" port receiver
        (fn [_ close-receiver]
          (nio/socket "127.0.0.1" 0 nil
            (fn [send-msg close-sender]
              (try
                (dotimes [i packet-count]
                  (send-msg "127.0.0.1" port (byte-array [i])))
                (let [result (deref done 3000 :timeout)]
                  (is (not= :timeout result))
                  (is (= [[0] [1] [2] [3] [4]] result)))
                (finally
                  (close-receiver)
                  (close-sender))))))))))

(deftest nio-multicast-socket-test
  (testing "multicast socket lifecycle and transmission (nio/socket)"
    (let [port (free-port)
          multicast-addr "224.0.0.251"
          received (promise)
          receiver (fn [host _ data]
                     (deliver received {:host host :data (vec data)}))
          payload (byte-array [99 98 97])]
      (nio/socket "0.0.0.0" port receiver
        (fn [receiver-send close-receiver]
          (is (fn? receiver-send))
          (is (fn? close-receiver))
          (nio/socket "0.0.0.0" 0 nil
            (fn [sender-send close-sender]
              (try
                (sender-send multicast-addr port payload)
                (let [result (deref received 2000 :timeout)]
                  (is (not= :timeout result))
                  (is (= [99 98 97] (:data result))))
                (finally
                  (close-receiver)
                  (close-sender))))))
        :multicast multicast-addr)))

  (testing "interop multicast: io socket sends and nio socket receives"
    (let [port (free-port)
          multicast-addr "224.0.0.251"
          received (promise)
          receiver (fn [host _ data]
                     (deliver received {:host host :data (vec data)}))
          payload (byte-array [11 22 33])]
      (nio/socket "0.0.0.0" port receiver
        (fn [receiver-send close-receiver]
          (io/socket "0.0.0.0" 0 nil
            (fn [sender-send close-sender]
              (try
                (sender-send multicast-addr port payload)
                (let [result (deref received 2000 :timeout)]
                  (is (not= :timeout result))
                  (is (= [11 22 33] (:data result))))
                (finally
                  (close-receiver)
                  (close-sender))))))
        :multicast multicast-addr)))

  (testing "interop multicast: nio socket sends and io socket receives"
    (let [port (free-port)
          multicast-addr "224.0.0.251"
          received (promise)
          receiver (fn [host _ data]
                     (deliver received {:host host :data (vec data)}))
          payload (byte-array [44 55 66])]
      (io/socket "0.0.0.0" port receiver
        (fn [receiver-send close-receiver]
          (nio/socket "0.0.0.0" 0 nil
            (fn [sender-send close-sender]
              (try
                (sender-send multicast-addr port payload)
                (let [result (deref received 2000 :timeout)]
                  (is (not= :timeout result))
                  (is (= [44 55 66] (:data result))))
                (finally
                  (close-receiver)
                  (close-sender))))))
        :multicast multicast-addr))))
