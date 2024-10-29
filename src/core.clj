(ns core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as str]
            [input]
            [interpolation :as inter])
  (:import (java.util Locale)))

(def algorithms
  {"linear"
   {:func inter/linear-interpolation
    :win-size 2
    :name "Linear"}

   "lagrange"
   {:func inter/lagrange-interpolation
    :win-size 4
    :name "Lagrange"}})

(def get-max-window-size (fn [alg-names] (reduce max (map #(get-in algorithms [% :win-size]) alg-names))))

(def cli-options
  [["-a" "--algorithms ALGORITHMS" "Comma-separated list of interpolation algorithms (linear,lagrange)"
    :multi true
    :default []
    :update-fn #(conj %1 (str/lower-case %2))
    :validate [#(contains? algorithms %) "Unknown algorithm specified"]]
   ["-s" "--step STEP" "Data sampling frequency"
    :parse-fn #(Double/parseDouble %)
    :validate [#(pos? %) "Step must be positive"]
    :default 1.0]
   ["-h" "--help"]])

(defn usage [options-summary]
  (->> ["Interpolator - A Clojure application for data interpolation."
        ""
        "Usage: interpolator [options]"
        ""
        "Options:"
        options-summary]
       (str/join \newline)))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n"
       (str/join \newline errors)))

(defn parse-args [args]
  (let [{:keys [options _ errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options) (exit 0 (usage summary))
      errors (exit 1 (error-msg errors))
      (empty? (:algorithms options)) (exit 1 "No algorithms provided")
      :else options)))

(defn- print-values [key result]
  (println (str/join "\t" (map #(String/format Locale/ENGLISH "%.2f" (into-array Object [(key %)])) result))))

(defn process-algorithm [alg step]
  (when (>= (count @input/points) (:win-size alg))
    (let [interpolated-points (inter/execute (:func alg) @input/points step (:win-size alg))]
      (println (str (:name alg) " interpolation result:"))
      (print-values :x interpolated-points)
      (print-values :y interpolated-points))))

(defn run-interpolation [alg-names step]
  (doseq [alg-name alg-names
          :let [alg (get algorithms alg-name)]]
    (process-algorithm alg step)))

(defn -main [& args]
  (let [{:keys [algorithms step]} (parse-args args)]
    (try
      (let [max-window-size (get-max-window-size algorithms)]
        (while true
          (input/read-point max-window-size)
          (run-interpolation algorithms step)))
      (catch Exception e
        (exit 1 (str "Error during interpolation: " (.getMessage e)))))))

(comment
  (do
    (input/read-point 2)
    (println @input/points)
    (println (inter/execute inter/linear-interpolation @input/points 1 2))))
