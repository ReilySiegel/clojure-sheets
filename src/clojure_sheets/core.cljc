(ns clojure-sheets.core
  (:require [ajax.core :as http]
            [clojure.core.async :as a]
            [clojure.tools.reader.edn :as edn]))

(defn- fetch-url [url chan]
  (http/GET url
            {:keywords?       true
             :handler         #(a/offer! chan %)
             :response-format :json})
  chan)

(defn- sheet-url
  [id page]
  (str "https://spreadsheets.google.com/feeds/cells/" id "/" page "/public/full?alt=json"))

(defn- parse-entry [{:keys [$t numericValue]
                     :as   cell}]
  cell
  (cond
    numericValue (edn/read-string numericValue)
    :else        $t))

(defn- row->vec [row]
  (loop [row        (sort-by (comp :gs$cell :col) row)
         index      1
         ;; Start with the id
         filled-row [(edn/read-string (get-in (first row) [:gs$cell :row]))]]
    (if-not (seq row)
      filled-row
      (if (= (str index) (get-in (first row) [:gs$cell :col]))
        (recur (rest row)
               (inc index)
               (conj filled-row (parse-entry (:gs$cell (first row)))))
        (recur row (inc index) (conj filled-row nil))))))

(def ^:private sheet->vec-xform
  (comp (map :feed)
        (map :entry)
        (map (partial group-by (comp :row :gs$cell)))
        (map (partial sort-by first))
        (map (partial map second))
        (map (partial map row->vec))))

(defn- zip [keys vals]
  (into {} (map vector keys vals)))

(defn- vecs->map [key-fn [headers & rows]]
  (into []
        (map (partial zip (cons ::row (map key-fn
                                           ;; Drop Row column 
                                           (rest headers)))))
        rows))

(defn sheet->vecs
  "Converts a page of a Google Sheet into a 2D Clojure Vector.
  Takes a `sheet-id` and a map of options.

  Options can include:
  - `:page` The page of the sheet to retrieve, defaults to 1."
  ([sheet-id] (sheet->vecs sheet-id {}))
  ([sheet-id
    {:keys [page]
     :or   {page 1}}]
   (fetch-url (sheet-url sheet-id page)
              (a/promise-chan sheet->vec-xform))))

(defn sheet->map
  "Converts a page of a Google Sheet into a Clojure map.
  Takes a `sheet-id` and a map of options.

  Options can include:
  - `:page` The page of the sheet to retrieve, defaults to 1.
  - `:key-fn` The function used to convert the headers to map keys, defaults
     to `identity`.

  Other `:key-fn`s are provided in `clojure-sheets.key-fns`, such as
  `idiomatic-keyword`."
  ([sheet-id] (sheet->map sheet-id {}))
  ([sheet-id
    {:keys [page key-fn]
     :or   {page  1
            key-fn identity}}]
   (fetch-url (sheet-url sheet-id page)
              (a/promise-chan (comp sheet->vec-xform
                                    (map (partial vecs->map key-fn)))))))
