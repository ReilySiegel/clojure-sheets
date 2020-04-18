(ns clojure-sheets.key-fns
  (:require [clojure.string :as str]))

(defn make-keyword [parts]
  (let [ns (butlast parts)
        n  (last parts)]
    (if ns
      (keyword (str/join "." ns) n)
      (keyword n))))

(defn idiomatic-keyword [header]
  (-> header
      (str/lower-case)
      (str/replace #"\s+" "-")
      (str/split #"[._/]")
      (make-keyword)))
