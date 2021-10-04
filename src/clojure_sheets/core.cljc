(ns clojure-sheets.core
  (:require [ajax.core :as http]
            #?(:clj [cheshire.core :as json])
            [clojure.core.async :as a]
            [clojure.tools.reader.edn :as edn]))

(defn- parse-json [string]
  #?(:clj (json/parse-string keyword string)
     :cljs (js->clj (. js/JSON parse string) :keywordize-keys true)))

(defn- fetch-url [url chan]
  (http/GET url
            {:response-format :text
             :handler
             #(a/offer! chan (->> %
                                  butlast
                                  butlast
                                  (drop 47)
                                  (apply str)
                                  parse-json))})
  chan)

(defn- sheet-url
  [id sheet-name]
  (str "https://docs.google.com/spreadsheets/d/"
       id "/gviz/tq?tqx=out:json&sheet=" sheet-name))

(defn- zip [keys vals]
  (into {} (map vector keys vals)))

(defn- table->map [key-fn {{:keys [cols rows]} :table}]
  (into [] (map (partial zip (map (comp key-fn :label) cols))
                (into []
                      (comp (map :c)
                            (map (partial map (comp (partial some identity)
                                                    (juxt :f :v))))
                            (map vec))
                      rows))))

(defn sheet->map
  "Converts a page of a Google Sheet into a Clojure map.
  Takes a `sheet-id` and a map of options.

  Options can include:
  - `:page` The page of the sheet to retrieve, defaults to 1.
  - `:key-fn` The function used to convert the headers to map keys, defaults
     to `identity`.

  Other `:key-fn`s are provided in `clojure-sheets.key-fns`, such as
  `idiomatic-keyword`."
  ([sheet-id page] (sheet->map sheet-id page {}))
  ([sheet-id page
    {:keys [key-fn]
     :or   {key-fn identity}}]
   (fetch-url (sheet-url sheet-id page)
              (a/promise-chan (map (partial table->map key-fn))))))
