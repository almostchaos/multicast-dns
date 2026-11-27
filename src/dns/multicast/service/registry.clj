(ns dns.multicast.service.registry
  (:gen-class)
  (:require [clojure.string :as string]))

(defn create-service-registry []
  (let [resources (atom {})

        lookup
        (fn [query]
          (filter
            (fn [[name _]]
              (and
                (not (empty? name))
                (string/ends-with? (string/lower-case name) query)))
            @resources))

        register
        (fn [service-type service-instance info]
          (let [name (string/lower-case (str service-instance "." service-type))]
            (swap! resources assoc name [service-type service-instance info])))]

    (fn [control] (control lookup register))))

(defn create-ip-registry []
  (let [resources (atom {})
        lookup
        (fn [hostname]
          (when hostname
            (get @resources (string/lower-case hostname))))

        register
        (fn [hostname ip]
          (swap! resources assoc (string/lower-case hostname) ip))]

    (fn [control] (control lookup register))))
