(ns input
  (:require [clojure.string :as str]
            [interpolation :refer [->Point]]))

(defn parse-point [input]
  (let [tokens (str/split input #"[,\t ;]+")]
    (if (str/blank? input) (do (println "Blank input received. Exiting.") (System/exit 0))
        (if (= (count tokens) 2)
          (let [[x y] tokens]
            (try
              (->Point (Double/parseDouble x) (Double/parseDouble y))
              (catch NumberFormatException e
                (throw (ex-info "Both coordinates must be valid numbers." {:input input} e)))))
          (throw (ex-info "Input must contain exactly two numerical values separated by comma, tab, space, or semicolon." {:input input}))))))

(defn read-input-seq
  "Lazily reads input lines from standard input and parses them into Point records."
  []
  (->> (line-seq (java.io.BufferedReader. *in*))
       (map str/trim)
       (map parse-point)
       (filter some?)))
