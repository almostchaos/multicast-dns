(ns socket.io.udp-test
  (:require [clojure.test :refer :all]
            [socket.io.udp :refer :all])
  (:import (java.net DatagramSocket)))

(defn- get-free-port []
  (with-open [s (new DatagramSocket 0)]
    (.getLocalPort s)))

(deftest max-payload-test
  (testing "max-payload is defined as 508 bytes"
    (is (= 508 max-payload))))

(deftest socket-structure-test
  (testing "socket returns a map with :send and :close functions"
    (let [{:keys [send close] :as s} (socket "127.0.0.1" 0 nil)]
      (try
        (is (map? s))
        (is (fn? send))
        (is (fn? close))
        (finally
          (close))))))

(deftest unicast-send-receive-test
  (testing "sending and receiving byte array over UDP"
    (let [port (get-free-port)
          received (promise)
          receiver (fn [host sender-port data]
                     (deliver received {:host host :port sender-port :data (vec data)}))
          {close-receiver :close} (socket "127.0.0.1" port receiver)
          {send-msg :send close-sender :close} (socket "127.0.0.1" 0 nil)
          payload (byte-array [10 20 30 40 50])]
      (try
        (send-msg "127.0.0.1" port payload)
        (let [result (deref received 2000 :timeout)]
          (is (not= :timeout result))
          (is (some? (:host result)))
          (is (pos? (:port result)))
          (is (= [10 20 30 40 50] (:data result))))
        (finally
          (close-receiver)
          (close-sender)))))

  (testing "sending string payload converted via byte-streams"
    (let [port (get-free-port)
          received (promise)
          receiver (fn [host sender-port data]
                     (deliver received {:host host :port sender-port :data (String. ^bytes data "UTF-8")}))
          {close-receiver :close} (socket "127.0.0.1" port receiver)
          {send-msg :send close-sender :close} (socket "127.0.0.1" 0 nil)
          message "hello udp test"]
      (try
        (send-msg "127.0.0.1" port message)
        (let [result (deref received 2000 :timeout)]
          (is (not= :timeout result))
          (is (= "hello udp test" (:data result))))
        (finally
          (close-receiver)
          (close-sender))))))

(deftest multiple-messages-test
  (testing "receiver loop handles multiple consecutive messages"
    (let [port (get-free-port)
          messages (atom [])
          received-all (promise)
          expected-count 3
          receiver (fn [_ _ data]
                     (swap! messages conj (vec data))
                     (when (= (count @messages) expected-count)
                       (deliver received-all @messages)))
          {close-receiver :close} (socket "127.0.0.1" port receiver)
          {send-msg :send close-sender :close} (socket "127.0.0.1" 0 nil)]
      (try
        (send-msg "127.0.0.1" port (byte-array [1 1]))
        (Thread/sleep 50)
        (send-msg "127.0.0.1" port (byte-array [2 2]))
        (Thread/sleep 50)
        (send-msg "127.0.0.1" port (byte-array [3 3]))
        (let [result (deref received-all 2000 :timeout)]
          (is (not= :timeout result))
          (is (= [[1 1] [2 2] [3 3]] result)))
        (finally
          (close-receiver)
          (close-sender))))))

(deftest payload-size-test
  (testing "receiving max-payload (508 bytes)"
    (let [port (get-free-port)
          received (promise)
          receiver (fn [_ _ data]
                     (deliver received (count data)))
          {close-receiver :close} (socket "127.0.0.1" port receiver)
          {send-msg :send close-sender :close} (socket "127.0.0.1" 0 nil)
          large-payload (byte-array (repeat max-payload (byte 42)))]
      (try
        (send-msg "127.0.0.1" port large-payload)
        (let [result (deref received 2000 :timeout)]
          (is (= max-payload result)))
        (finally
          (close-receiver)
          (close-sender)))))

  (testing "receiving payload larger than max-payload truncates to max-payload"
    (let [port (get-free-port)
          received (promise)
          receiver (fn [_ _ data]
                     (deliver received (count data)))
          {close-receiver :close} (socket "127.0.0.1" port receiver)
          {send-msg :send close-sender :close} (socket "127.0.0.1" 0 nil)
          oversized-payload (byte-array (repeat (+ max-payload 100) (byte 7)))]
      (try
        (send-msg "127.0.0.1" port oversized-payload)
        (let [result (deref received 2000 :timeout)]
          (is (= max-payload result)))
        (finally
          (close-receiver)
          (close-sender)))))

  (testing "receiving 0 byte empty payload"
    (let [port (get-free-port)
          received (promise)
          receiver (fn [_ _ data]
                     (deliver received (count data)))
          {close-receiver :close} (socket "127.0.0.1" port receiver)
          {send-msg :send close-sender :close} (socket "127.0.0.1" 0 nil)
          empty-payload (byte-array 0)]
      (try
        (send-msg "127.0.0.1" port empty-payload)
        (let [result (deref received 2000 :timeout)]
          (is (= 0 result)))
        (finally
          (close-receiver)
          (close-sender))))))

(deftest nil-receiver-and-address-test
  (testing "socket with nil receiver works for sending and closes cleanly"
    (let [{send-msg :send close-sock :close} (socket "127.0.0.1" 0 nil)]
      (is (nil? (send-msg "127.0.0.1" 9999 (byte-array [1 2 3]))))
      (is (nil? (close-sock)))))

  (testing "socket with nil address does not start receiver loop"
    (let [{close-sock :close :as s} (socket nil 0 (fn [& _]))]
      (is (map? s))
      (is (nil? (close-sock))))))

(deftest close-socket-test
  (testing "closing socket is idempotent and sending on closed socket is a safe no-op"
    (let [{send-msg :send close-sock :close} (socket "127.0.0.1" 0 nil)]
      (close-sock)
      ;; Repeated close
      (is (nil? (close-sock)))
      ;; Send after close
      (is (nil? (send-msg "127.0.0.1" 9999 (byte-array [1 2 3]))))))

  (testing "closed receiver socket stops receiving"
    (let [port (get-free-port)
          messages (atom [])
          receiver (fn [_ _ data]
                     (swap! messages conj (vec data)))
          {close-receiver :close} (socket "127.0.0.1" port receiver)
          {send-msg :send close-sender :close} (socket "127.0.0.1" 0 nil)]
      (close-receiver)
      (Thread/sleep 100)
      (send-msg "127.0.0.1" port (byte-array [1 2 3]))
      (Thread/sleep 200)
      (is (empty? @messages))
      (close-sender))))

(deftest multicast-socket-test
  (testing "creating and closing multicast socket"
    (let [port (get-free-port)
          {:keys [send close] :as s} (socket "0.0.0.0" port (fn [& _]) :multicast "224.0.0.251")]
      (try
        (is (map? s))
        (is (fn? send))
        (is (fn? close))
        (finally
          (close))))))
