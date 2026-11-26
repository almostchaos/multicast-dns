(ns dns.message-test
  (:require [clojure.test :refer :all]
            [dns.message :refer :all]
            [dns.encoding :as encoding]))

(deftest ptr-query-test
  (let [service-path "_googlecast._tcp.local"
        message (ptr-query service-path)
        decoded (encoding/decode-message message)
        header (first decoded)
        question (second decoded)]
    (is (= 1 (:QDCOUNT header)))
    (is (= 0 (:ANCOUNT header)))
    (is (= service-path (:QNAME question)))
    (is (= encoding/type:PTR (:QTYPE question)))
    (is (= encoding/class:IN (:QCCLASS question)))))

(deftest a-query-test
  (let [name "googlecast.local"
        message (a-query name)
        decoded (encoding/decode-message message)
        header (first decoded)
        question (second decoded)]
    (is (= 1 (:QDCOUNT header)))
    (is (= 0 (:ANCOUNT header)))
    (is (= name (:QNAME question)))
    (is (= encoding/type:A (:QTYPE question)))
    (is (= encoding/class:IN (:QCCLASS question)))))

(deftest ptr-answer-test
  (testing "PTR answer with TXT"
    (let [service-path "_googlecast._tcp.local"
          instance "my-instance"
          hostname "my-host.local"
          port 8008
          txt {:id "123"}
          message (ptr-answer service-path instance {:host hostname :port port :txt txt})
          decoded (encoding/decode-message message)
          header (first decoded)
          answers (rest decoded)]
      (is (= 1 (:ANCOUNT header)))
      (is (= 2 (:ARCOUNT header)))
      (is (= 3 (count answers)))
      
      (let [ptr (first answers)
            srv (second answers)
            txt-record (nth answers 2)]
        (is (= service-path (:NAME ptr)))
        (is (= encoding/type:PTR (:TYPE ptr)))
        (is (= (str instance "." service-path) (:RDATA ptr)))
        
        (is (= (str instance "." service-path) (:NAME srv)))
        (is (= encoding/type:SRV (:TYPE srv)))
        (is (= port (get-in srv [:RDATA :port])))
        (is (= "my-host.local" (get-in srv [:RDATA :host])))
        
        (is (= (str instance "." service-path) (:NAME txt-record)))
        (is (= encoding/type:TXT (:TYPE txt-record)))
        (is (= {"id" "123"} (:RDATA txt-record))))))
  
  (testing "PTR answer without TXT"
    (let [service-path "_googlecast._tcp.local"
          instance "my-instance"
          hostname "my-host.local"
          port 8008
          message (ptr-answer service-path instance {:host hostname :port port})
          decoded (encoding/decode-message message)
          header (first decoded)
          answers (rest decoded)]
      (is (= 1 (:ANCOUNT header)))
      (is (= 1 (:ARCOUNT header)))
      (is (= 2 (count answers)))
      (is (= encoding/type:PTR (:TYPE (first answers))))
      (is (= encoding/type:SRV (:TYPE (second answers))))))

  (testing "PTR answer with IP includes A record"
    (let [service-path "_googlecast._tcp.local"
          instance "my-instance"
          hostname "my-host.local"
          port 8008
          ip ["192" "168" "0" "244"]
          message (ptr-answer service-path instance {:host hostname :port port :ip ip})
          decoded (encoding/decode-message message)
          header (first decoded)
          answers (rest decoded)]
      (is (= 1 (:ANCOUNT header)))
      (is (= 2 (:ARCOUNT header)))
      (is (= 3 (count answers)))
      (is (= encoding/type:PTR (:TYPE (first answers))))
      (is (= encoding/type:SRV (:TYPE (second answers))))
      (let [a-record (nth answers 2)]
        (is (= hostname (:NAME a-record)))
        (is (= encoding/type:A (:TYPE a-record)))
        (is (= "192.168.0.244" (:RDATA a-record))))))

  (testing "PTR answer with TXT, IP, and custom TTL"
    (let [service-path "_googlecast._tcp.local"
          instance "my-instance"
          hostname "my-host.local"
          port 8008
          txt {:id "123"}
          ip ["192" "168" "0" "244"]
          message (ptr-answer service-path instance {:host hostname :port port :txt txt :ip ip :ttl 300})
          decoded (encoding/decode-message message)
          header (first decoded)
          answers (rest decoded)]
      (is (= 1 (:ANCOUNT header)))
      (is (= 3 (:ARCOUNT header)))
      (is (= 4 (count answers)))
      (is (= encoding/type:PTR (:TYPE (first answers))))
      (is (= encoding/type:SRV (:TYPE (second answers))))
      (is (= encoding/type:TXT (:TYPE (nth answers 2))))
      (is (= encoding/type:A (:TYPE (nth answers 3))))
      (is (every? #(= 300 (:TTL %)) answers)))))

(deftest srv-answer-test
  (testing "SRV answer with TXT"
    (let [service-path "_googlecast._tcp.local"
          instance "my-instance"
          hostname "my-host.local"
          port 8008
          txt {:id "123"}
          message (srv-answer service-path instance {:host hostname :port port :txt txt})
          decoded (encoding/decode-message message)
          header (first decoded)
          answers (rest decoded)]
      (is (= 1 (:ANCOUNT header)))
      (is (= 1 (:ARCOUNT header)))
      (is (= 2 (count answers)))
      (is (= encoding/type:SRV (:TYPE (first answers))))
      (is (= encoding/type:TXT (:TYPE (second answers))))))
  
  (testing "SRV answer without TXT"
    (let [service-path "_googlecast._tcp.local"
          instance "my-instance"
          hostname "my-host.local"
          port 8008
          message (srv-answer service-path instance {:host hostname :port port})
          decoded (encoding/decode-message message)
          header (first decoded)
          answers (rest decoded)]
      (is (= 1 (:ANCOUNT header)))
      (is (= 0 (:ARCOUNT header)))
      (is (= 1 (count answers)))
      (is (= encoding/type:SRV (:TYPE (first answers))))))

  (testing "SRV answer with IP includes A record"
    (let [service-path "_googlecast._tcp.local"
          instance "my-instance"
          hostname "my-host.local"
          port 8008
          ip ["192" "168" "0" "244"]
          message (srv-answer service-path instance {:host hostname :port port :ip ip})
          decoded (encoding/decode-message message)
          header (first decoded)
          answers (rest decoded)]
      (is (= 1 (:ANCOUNT header)))
      (is (= 1 (:ARCOUNT header)))
      (is (= 2 (count answers)))
      (is (= encoding/type:SRV (:TYPE (first answers))))
      (let [a-record (second answers)]
        (is (= hostname (:NAME a-record)))
        (is (= encoding/type:A (:TYPE a-record)))
        (is (= "192.168.0.244" (:RDATA a-record)))))))

(deftest txt-answer-test
  (testing "TXT answer"
    (let [service-path "_googlecast._tcp.local"
          instance "my-instance"
          txt {:id "123"}
          message (txt-answer service-path instance {:txt txt})
          decoded (encoding/decode-message message)
          header (first decoded)
          answers (rest decoded)]
      (is (= 1 (:ANCOUNT header)))
      (is (= 1 (:ARCOUNT header)))
      (is (= 1 (count answers)))
      (is (= encoding/type:TXT (:TYPE (first answers))))
      (is (= {"id" "123"} (:RDATA (first answers))))))
  
  (testing "Empty TXT answer"
    (let [service-path "_googlecast._tcp.local"
          instance "my-instance"
          message (txt-answer service-path instance {:txt {}})
          decoded (encoding/decode-message message)
          header (first decoded)
          answers (rest decoded)]
      (is (= 1 (:ANCOUNT header)))
      (is (= 1 (count answers)))
      (is (= encoding/type:TXT (:TYPE (first answers))))
      (is (= {} (:RDATA (first answers))))))

  (testing "Nil TXT answer"
    (doseq [info [{} nil]]
      (let [service-path "_googlecast._tcp.local"
            instance "my-instance"
            message (txt-answer service-path instance info)
            decoded (encoding/decode-message message)
            header (first decoded)
            answers (rest decoded)]
        (is (= 1 (:ANCOUNT header)))
        (is (= 1 (count answers)))
        (is (= encoding/type:TXT (:TYPE (first answers))))
        (is (= {} (:RDATA (first answers))))))))

(deftest a-answer-test
  (testing "A answer with default TTL"
    (let [message (a-answer nil nil {:host ["my-host" "local"] :ip ["192" "168" "0" "244"]})
          decoded (encoding/decode-message message)
          header (first decoded)
          answers (rest decoded)
          a-record (first answers)]
      (is (= 1 (:ANCOUNT header)))
      (is (= 1 (count answers)))
      (is (= "my-host.local" (:NAME a-record)))
      (is (= encoding/type:A (:TYPE a-record)))
      (is (= 120 (:TTL a-record)))
      (is (= "192.168.0.244" (:RDATA a-record)))))

  (testing "A answer with custom TTL"
    (let [message (a-answer nil nil {:host ["my-host" "local"] :ip ["10" "0" "0" "1"] :ttl 60})
          decoded (encoding/decode-message message)
          a-record (second decoded)]
      (is (= "my-host.local" (:NAME a-record)))
      (is (= encoding/type:A (:TYPE a-record)))
      (is (= 60 (:TTL a-record)))
      (is (= "10.0.0.1" (:RDATA a-record))))))
