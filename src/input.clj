(ns input
  (:require [clojure.string :as str]
            [interpolation :refer [->Point]]))

(def points (ref []))

(defn- parse-point [input]
  (let [tokens (str/split input #"[,\t ;]+")]
    (if (= (count tokens) 2)
      (let [[x y] tokens]
        (try
          (->Point (Double/parseDouble x) (Double/parseDouble y))
          (catch NumberFormatException e
            (throw (ex-info "Both coordinates must be valid numbers." {:input input} e)))))
      (throw (ex-info "Input must contain exactly two numerical values separated by comma, tab, space, or semicolon." {:input input})))))

(defn read-input []
  (loop []
    (let [line (read-line)]
      (cond
        (nil? line) (do (println "No input received. Exiting.") (System/exit 0))
        (str/blank? (str/trim line)) (do (println "Blank input received. Exiting.") (System/exit 0))
        :else
        (let [parsed-point
              (try
                (parse-point (str/trim line))
                (catch Exception e
                  (println "Error parsing point:" (.getMessage e))
                  (println "Please enter the point again (e.g., 1,2):")
                  ::error))]
          (if (= parsed-point ::error)
            (recur)
            parsed-point))))))

(defn read-point [win-size]
  (println "Enter point:")
  (dosync
   (alter points conj (read-input))
   (when (> (count @points) win-size)
     (alter points #(vec (rest %))))))


(comment
  (parse-point "1,2")
  (parse-point "1;2;3")
  (parse-point "1\t2\t3")
  (parse-point "1 2 3")
  (parse-point "1-2-3"))
