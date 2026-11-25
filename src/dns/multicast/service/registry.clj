(ns dns.multicast.service.registry
  (:gen-class)
  (:require [clojure.string :as string]))

(defn create-service-registry []
  (let [resources (atom {})

        lookup
        (fn [query]
          (filter (fn [[name _]] (string/ends-with? name query)) @resources))

        register
        (fn [service-type service-instance info]
          (let [name (str service-instance "." service-type)]
            (swap! resources assoc name [service-type service-instance info])))]

    (fn [control] (control lookup register))))

(defn create-ip-registry []
  (let [resources (atom {})
        lookup
        (fn [hostname]
          (get @resources hostname))

        register
        (fn [hostname ip]
          (swap! resources assoc hostname ip))]

    (fn [control] (control lookup register))))
