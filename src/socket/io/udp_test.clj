(ns socket.io.udp-test
  (:require [clojure.test :refer :all]
            [socket.io.udp :as udp])
  (:import (java.net ServerSocket)))


(deftest udp-socket-send-receive-test
  (let [port (with-open [socket (new ServerSocket 0)]
               (.getLocalPort socket))
        address "127.0.0.1"
        received-data (promise)
        receiver (fn [_ _ data] (deliver received-data data))
        {send :send close :close} (udp/socket address port receiver)
        message (byte-array [1 2 3 4])]
    (try
      (send address port message)
      (let [received (deref received-data 2000 :timeout)]
        (is (not= :timeout received) "Should have received data")
        (is (java.util.Arrays/equals ^bytes message ^bytes received)))
      (finally
        (close)))))

(deftest udp-close-test
  (let [port (with-open [socket (new ServerSocket 0)]
               (.getLocalPort socket))
        address "127.0.0.1"
        receiver (fn [_ _ _] nil)
        {close :close} (udp/socket address port receiver)]
    (is (nil? (close)) "Closing should return nil and not throw exception")))
