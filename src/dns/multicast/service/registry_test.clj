(ns dns.multicast.service.registry-test
  (:require [clojure.test :refer :all]
            [dns.multicast.service.registry :refer [create-registry]]))

(deftest create-registry-test
  (testing "creates a registry control function that passes lookup and register"
    (let [registry (create-registry)]
      (is (fn? registry))
      (registry
        (fn [lookup register]
          (is (fn? lookup))
          (is (fn? register))))))

  (testing "registry returns the result of the control function"
    (let [registry (create-registry)
          result (registry (fn [_ _] :custom-result))]
      (is (= :custom-result result)))))

(deftest empty-registry-lookup-test
  (testing "lookup on empty registry returns empty sequence"
    (let [registry (create-registry)]
      (registry
        (fn [lookup _]
          (is (empty? (lookup "anything")))
          (is (empty? (lookup "")))
          (is (empty? (lookup "_http._tcp.local"))))))))

(deftest register-and-lookup-test
  (testing "register and lookup by exact match"
    (let [registry (create-registry)
          service-name "my-service._http._tcp.local"
          service-info ["_http._tcp.local" "my-service" :host "127.0.0.1" :port 8080]]
      (registry
        (fn [lookup register]
          (register service-name service-info)
          (let [results (into {} (lookup service-name))]
            (is (= 1 (count results)))
            (is (= service-info (get results service-name))))))))

  (testing "lookup by service type suffix"
    (let [registry (create-registry)
          service-name "my-service._http._tcp.local"
          service-info ["_http._tcp.local" "my-service" :host "127.0.0.1" :port 8080]]
      (registry
        (fn [lookup register]
          (register service-name service-info)
          (is (= {service-name service-info} (into {} (lookup "_http._tcp.local"))))
          (is (= {service-name service-info} (into {} (lookup ".local"))))
          (is (= {service-name service-info} (into {} (lookup "local"))))
          (is (= {service-name service-info} (into {} (lookup ""))))))))

  (testing "lookup non-matching query returns empty"
    (let [registry (create-registry)]
      (registry
        (fn [lookup register]
          (register "my-service._http._tcp.local" {:port 8080})
          (is (empty? (lookup "_googlecast._tcp.local")))
          (is (empty? (lookup "_http._tcp")))
          (is (empty? (lookup "my-service"))))))))

(deftest multiple-resources-lookup-test
  (testing "filters resources ending with query"
    (let [registry (create-registry)
          http-1 "instance-1._http._tcp.local"
          http-2 "instance-2._http._tcp.local"
          ssh-1  "instance-1._ssh._tcp.local"
          cast-1 "instance-1._googlecast._tcp.local"
          info-http-1 {:type :http :id 1}
          info-http-2 {:type :http :id 2}
          info-ssh-1  {:type :ssh :id 1}
          info-cast-1 {:type :cast :id 1}]
      (registry
        (fn [lookup register]
          (register http-1 info-http-1)
          (register http-2 info-http-2)
          (register ssh-1 info-ssh-1)
          (register cast-1 info-cast-1)

          (testing "matching http services"
            (let [http-results (into {} (lookup "_http._tcp.local"))]
              (is (= 2 (count http-results)))
              (is (= info-http-1 (get http-results http-1)))
              (is (= info-http-2 (get http-results http-2)))))

          (testing "matching ssh services"
            (let [ssh-results (into {} (lookup "_ssh._tcp.local"))]
              (is (= 1 (count ssh-results)))
              (is (= info-ssh-1 (get ssh-results ssh-1)))))

          (testing "matching domain suffix"
            (let [all-local (into {} (lookup ".local"))]
              (is (= 4 (count all-local)))))

          (testing "empty string query matches all registered resources"
            (let [all (into {} (lookup ""))]
              (is (= 4 (count all))))))))))

(deftest overwrite-registration-test
  (testing "registering with existing name updates its info"
    (let [registry (create-registry)
          service-name "printer._ipp._tcp.local"
          initial-info {:port 631 :status "idle"}
          updated-info {:port 631 :status "printing"}]
      (registry
        (fn [lookup register]
          (register service-name initial-info)
          (is (= {service-name initial-info} (into {} (lookup service-name))))

          (register service-name updated-info)
          (let [results (into {} (lookup service-name))]
            (is (= 1 (count results)))
            (is (= updated-info (get results service-name)))))))))

(deftest registry-isolation-test
  (testing "distinct registry instances are isolated from each other"
    (let [reg1 (create-registry)
          reg2 (create-registry)]
      (reg1 (fn [_ register] (register "svc-1._http._tcp.local" {:reg 1})))
      (reg2 (fn [_ register] (register "svc-2._http._tcp.local" {:reg 2})))

      (is (= 1 (count (reg1 (fn [lookup _] (lookup ".local"))))))
      (is (= 1 (count (reg2 (fn [lookup _] (lookup ".local"))))))

      (is (seq (reg1 (fn [lookup _] (lookup "svc-1._http._tcp.local")))))
      (is (empty? (reg1 (fn [lookup _] (lookup "svc-2._http._tcp.local")))))

      (is (seq (reg2 (fn [lookup _] (lookup "svc-2._http._tcp.local")))))
      (is (empty? (reg2 (fn [lookup _] (lookup "svc-1._http._tcp.local"))))))))

(deftest concurrent-registration-test
  (testing "concurrent registrations from multiple threads are thread-safe"
    (let [registry (create-registry)
          n 100
          tasks (mapv (fn [i]
                        (future
                          (registry
                            (fn [_ register]
                              (register (str "instance-" i "._service._tcp.local") {:index i})))))
                      (range n))]
      (doseq [t tasks]
        @t)
      (registry
        (fn [lookup _]
          (let [results (into {} (lookup "_service._tcp.local"))]
            (is (= n (count results)))))))))
