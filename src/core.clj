(ns core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as str]
            [input :refer [read-input-seq]]
            [interpolation :refer [linear-interpolation
                                   lagrange-interpolation
                                   execute]]))

(def algorithms
  {"linear"
   {:func linear-interpolation
    :win-size 2
    :name "Linear"}

   "lagrange"
   {:func lagrange-interpolation
    :win-size 4
    :name "Lagrange"}})

(def cli-options
  [["-a" "--algorithms ALGORITHMS" "Comma-separated list of interpolation algorithms (linear,lagrange)"
    :default []
    :parse-fn #(mapv str/lower-case (map str/trim (str/split % #",")))
    :validate [#(every? algorithms %) "Unknown algorithm specified"]]
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

(defn process-algorithm [alg step points]
  (when (>= (count points) (:win-size alg))
    (let [interpolated-points (execute (:func alg) points step (:win-size alg))]
      (when (not-empty interpolated-points)
        (let [last-segment (last interpolated-points)
              xs (map :x last-segment)
              ys (map :y last-segment)
              formatted-xs (map #(format "%.2f" %) xs)
              formatted-ys (map #(format "%.2f" %) ys)]
          (println (str (:name alg) " interpolation result:"))
          (println (str/join "\t" formatted-xs))
          (println (str/join "\t" formatted-ys)))))))

(defn run-interpolation [alg-names step points]
  (doseq [alg-name alg-names
          :let [alg (get algorithms alg-name)]]
    (process-algorithm alg step points)))

(defn -main [& args]
  (let [{:keys [algorithms step]} (parse-args args)]
    (try
      (let [points-seq (read-input-seq)
            accumulated-points (reductions conj [] points-seq)]
        (doseq [points accumulated-points]
          (run-interpolation algorithms step points)))
      (catch Exception e
        (exit 1 (str "Error during interpolation: " (.getMessage e)))))))
