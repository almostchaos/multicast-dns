(ns dns.multicast.service.registry-test
  (:require [clojure.test :refer :all]
            [dns.multicast.service.registry :refer [create-ip-registry create-service-registry]]))

(deftest create-service-registry-test
  (testing "creates a registry control function that passes lookup and register"
    (let [registry (create-service-registry)]
      (is (fn? registry))
      (registry
        (fn [lookup register]
          (is (fn? lookup))
          (is (fn? register))))))

  (testing "registry returns the result of the control function"
    (let [registry (create-service-registry)
          result (registry (fn [_ _] :custom-result))]
      (is (= :custom-result result)))))

(deftest empty-service-registry-lookup-test
  (testing "lookup on empty registry returns empty sequence"
    (let [registry (create-service-registry)]
      (registry
        (fn [lookup _]
          (is (empty? (lookup "anything")))
          (is (empty? (lookup "")))
          (is (empty? (lookup "_http._tcp.local"))))))))

(deftest register-and-lookup-test
  (testing "register and lookup by exact match"
    (let [registry (create-service-registry)
          service-type "_http._tcp.local"
          service-instance "my-service"
          service-name (str service-instance "." service-type)
          info {:host "127.0.0.1" :port 8080}
          expected [service-type service-instance info]]
      (registry
        (fn [lookup register]
          (register service-type service-instance info)
          (let [results (into {} (lookup service-name))]
            (is (= 1 (count results)))
            (is (= expected (get results service-name))))))))

  (testing "lookup by service type suffix"
    (let [registry (create-service-registry)
          service-type "_http._tcp.local"
          service-instance "my-service"
          service-name (str service-instance "." service-type)
          info {:host "127.0.0.1" :port 8080}
          expected [service-type service-instance info]]
      (registry
        (fn [lookup register]
          (register service-type service-instance info)
          (is (= {service-name expected} (into {} (lookup "_http._tcp.local"))))
          (is (= {service-name expected} (into {} (lookup ".local"))))
          (is (= {service-name expected} (into {} (lookup "local"))))
          (is (= {service-name expected} (into {} (lookup ""))))))))

  (testing "lookup non-matching query returns empty"
    (let [registry (create-service-registry)]
      (registry
        (fn [lookup register]
          (register "_http._tcp.local" "my-service" {:port 8080})
          (is (empty? (lookup "_googlecast._tcp.local")))
          (is (empty? (lookup "_http._tcp")))
          (is (empty? (lookup "my-service"))))))))

(deftest multiple-resources-lookup-test
  (testing "filters resources ending with query"
    (let [registry (create-service-registry)
          http-1 "instance-1._http._tcp.local"
          http-2 "instance-2._http._tcp.local"
          ssh-1  "instance-1._ssh._tcp.local"
          info-http-1 {:type :http :id 1}
          info-http-2 {:type :http :id 2}
          info-ssh-1  {:type :ssh :id 1}
          info-cast-1 {:type :cast :id 1}]
      (registry
        (fn [lookup register]
          (register "_http._tcp.local" "instance-1" info-http-1)
          (register "_http._tcp.local" "instance-2" info-http-2)
          (register "_ssh._tcp.local" "instance-1" info-ssh-1)
          (register "_googlecast._tcp.local" "instance-1" info-cast-1)

          (testing "matching http services"
            (let [http-results (into {} (lookup "_http._tcp.local"))]
              (is (= 2 (count http-results)))
              (is (= ["_http._tcp.local" "instance-1" info-http-1] (get http-results http-1)))
              (is (= ["_http._tcp.local" "instance-2" info-http-2] (get http-results http-2)))))

          (testing "matching ssh services"
            (let [ssh-results (into {} (lookup "_ssh._tcp.local"))]
              (is (= 1 (count ssh-results)))
              (is (= ["_ssh._tcp.local" "instance-1" info-ssh-1] (get ssh-results ssh-1)))))

          (testing "matching domain suffix"
            (let [all-local (into {} (lookup ".local"))]
              (is (= 4 (count all-local)))))

          (testing "empty string query matches all registered resources"
            (let [all (into {} (lookup ""))]
              (is (= 4 (count all))))))))))

(deftest overwrite-registration-test
  (testing "registering with existing name updates its info"
    (let [registry (create-service-registry)
          service-type "_ipp._tcp.local"
          service-instance "printer"
          service-name (str service-instance "." service-type)
          initial-info {:port 631 :status "idle"}
          updated-info {:port 631 :status "printing"}]
      (registry
        (fn [lookup register]
          (register service-type service-instance initial-info)
          (is (= {service-name [service-type service-instance initial-info]}
                 (into {} (lookup service-name))))

          (register service-type service-instance updated-info)
          (let [results (into {} (lookup service-name))]
            (is (= 1 (count results)))
            (is (= [service-type service-instance updated-info]
                   (get results service-name)))))))))

(deftest registry-isolation-test
  (testing "distinct registry instances are isolated from each other"
    (let [reg1 (create-service-registry)
          reg2 (create-service-registry)]
      (reg1 (fn [_ register] (register "_http._tcp.local" "svc-1" {:reg 1})))
      (reg2 (fn [_ register] (register "_http._tcp.local" "svc-2" {:reg 2})))

      (is (= 1 (count (reg1 (fn [lookup _] (lookup ".local"))))))
      (is (= 1 (count (reg2 (fn [lookup _] (lookup ".local"))))))

      (is (seq (reg1 (fn [lookup _] (lookup "svc-1._http._tcp.local")))))
      (is (empty? (reg1 (fn [lookup _] (lookup "svc-2._http._tcp.local")))))

      (is (seq (reg2 (fn [lookup _] (lookup "svc-2._http._tcp.local")))))
      (is (empty? (reg2 (fn [lookup _] (lookup "svc-1._http._tcp.local"))))))))

(deftest concurrent-registration-test
  (testing "concurrent registrations from multiple threads are thread-safe"
    (let [registry (create-service-registry)
          n 100
          tasks (mapv (fn [i]
                        (future
                          (registry
                            (fn [_ register]
                              (register "_service._tcp.local" (str "instance-" i) {:index i})))))
                      (range n))]
      (doseq [t tasks]
        @t)
      (registry
        (fn [lookup _]
          (let [results (into {} (lookup "_service._tcp.local"))]
            (is (= n (count results)))))))))

(deftest create-ip-registry-test
  (testing "creates an IP registry control function that passes lookup and register"
    (let [registry (create-ip-registry)]
      (is (fn? registry))
      (registry
        (fn [lookup register]
          (is (fn? lookup))
          (is (fn? register))))))

  (testing "IP registry returns the result of the control function"
    (let [registry (create-ip-registry)
          result (registry (fn [_ _] :custom-result))]
      (is (= :custom-result result)))))

(deftest empty-ip-registry-lookup-test
  (testing "lookup on empty IP registry returns nil"
    (let [registry (create-ip-registry)]
      (registry
        (fn [lookup _]
          (is (nil? (lookup "test.local")))
          (is (nil? (lookup ""))))))))

(deftest ip-register-and-lookup-test
  (testing "register and lookup hostname to IP"
    (let [registry (create-ip-registry)
          hostname "test.local"
          ip ["192" "168" "0" "244"]]
      (registry
        (fn [lookup register]
          (register hostname ip)
          (is (= ip (lookup hostname)))
          (is (nil? (lookup "other.local")))
          (is (nil? (lookup ".local"))))))))

(deftest ip-overwrite-registration-test
  (testing "registering an existing hostname updates its IP"
    (let [registry (create-ip-registry)
          hostname "test.local"
          initial-ip ["192" "168" "0" "10"]
          updated-ip ["192" "168" "0" "244"]]
      (registry
        (fn [lookup register]
          (register hostname initial-ip)
          (is (= initial-ip (lookup hostname)))

          (register hostname updated-ip)
          (is (= updated-ip (lookup hostname))))))))

(deftest ip-registry-isolation-test
  (testing "distinct IP registry instances are isolated from each other"
    (let [reg1 (create-ip-registry)
          reg2 (create-ip-registry)]
      (reg1 (fn [_ register] (register "host-1.local" ["10" "0" "0" "1"])))
      (reg2 (fn [_ register] (register "host-2.local" ["10" "0" "0" "2"])))

      (is (= ["10" "0" "0" "1"] (reg1 (fn [lookup _] (lookup "host-1.local")))))
      (is (nil? (reg1 (fn [lookup _] (lookup "host-2.local")))))

      (is (= ["10" "0" "0" "2"] (reg2 (fn [lookup _] (lookup "host-2.local")))))
      (is (nil? (reg2 (fn [lookup _] (lookup "host-1.local"))))))))

(deftest ip-concurrent-registration-test
  (testing "concurrent IP registrations from multiple threads are thread-safe"
    (let [registry (create-ip-registry)
          n 100
          tasks (mapv (fn [i]
                        (future
                          (registry
                            (fn [_ register]
                              (register (str "host-" i ".local") ["10" "0" "0" (str i)])))))
                      (range n))]
      (doseq [t tasks]
        @t)
      (registry
        (fn [lookup _]
          (doseq [i (range n)]
            (is (= ["10" "0" "0" (str i)] (lookup (str "host-" i ".local"))))))))))
