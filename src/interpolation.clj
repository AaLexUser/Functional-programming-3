(ns interpolation)
(defrecord Point [x y])

(defn linear-interpolation
  "Performs linear interpolation between the last two points for a given x."
  [points x]
  (let [[p0 p1] (take-last 2 points)
        x0 (:x p0)
        x1 (:x p1)
        y0 (:y p0)
        y1 (:y p1)]
    (+ (* (- y1 y0) (/ (- x x0) (- x1 x0))) y0)))

(defn lagrange-basis
  "Calculates the Lagrange basis polynomial for a given point x."
  [points i x]
  (reduce
   (fn [acc j]
     (if (= i j)
       acc
       (/ (* acc (- x (:x (nth points j))))
          (- (:x (nth points i)) (:x (nth points j))))))
   1
   (range (count points))))

(defn lagrange-interpolation
  "Performs Lagrange interpolation between the last four points for a given x."
  [points x]
  (reduce
   (fn [acc i]
     (+ acc (* (lagrange-basis points i x) (:y (nth points i)))))
   0
   (range (count points))))

(defn generate-steps
  "Generates a sequence of steps from x-min to x-max with the specified step size."
  [x-min x-max step]
  (let [steps (take-while #(<= % x-max)
                          (iterate #(+ % step) x-min))
        last-step (+ (last steps) step)]
    (if (= (last steps) x-max)
      (vec steps)
      (conj (vec steps) last-step))))

(defn execute
  "Interpolates a range of points using the specified interpolation function."
  [interpolation-fn points step win-size]
  (let [bounds (take-last win-size points)
        left (:x (first bounds))
        right (:x (last bounds))
        steps (generate-steps left right step)]
    (mapv #(->Point % (interpolation-fn bounds %)) steps)))

(comment
  (generate-steps 0 1.571 1)
  (def points [(Point. 0 0) (Point. 1.571 1)])
  (execute linear-interpolation points 1 2))
