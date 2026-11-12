(ns dns.multicast.service.registry
  (:gen-class)
  (:require [clojure.string :as string]))

(defn create-registry []
  (let [resources (atom {})

        lookup
        (fn [query]
          (filter (fn [[name _]] (string/ends-with? name query)) @resources))

        register
        (fn [name info]
          (swap! resources assoc name info))]

    (fn [control] (control lookup register))))
