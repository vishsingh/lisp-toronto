; Sudoku solver, implemented in Clojure
; By Vish Singh
; April 3, 2009

(ns sudoku
  (:use clojure.set
	[clojure.contrib.str-utils :only (str-join)]))

(defn map2d-xy [f mat]
  (let [map-n (fn [f v] (vec (map f v (iterate inc 0))))]
    (map-n (fn [row y] (map-n (fn [elem x] (f elem x y)) row)) mat)))

(defn map2d [f mat]
  (map2d-xy (fn [elem x y] (f elem)) mat))

(defn strings-to-matrix [strlist]
  (map2d (comp #(if (zero? %) (set (range 1 10)) %) #(- (int %) (int \0))) strlist))

(defn print-matrix [mat]
  (doseq [row mat]
    (println (str-join " " (for [n row]
			     (if (set? n)
			       (apply str (for [x (range 1 10)] (if (n x) (char (+ x (int \0))) \_)))
			       (format "%9d" n)))))))

(defn print-matrices [mats]
  (doseq [mat mats]
    (print-matrix mat)
    (println (apply str (repeat 97 \-)))))

(defn remove-numbers-from-block [mat [x1 y1 x2 y2] numbers]
  (map2d-xy (fn [elem x y]
	      (if (and (>= x x1) (< x x2) (>= y y1) (< y y2) (set? elem))
		(difference elem numbers)
		elem))
	    mat))

(defn parse-block [mat [x1 y1 x2 y2 :as block]]
  (remove-numbers-from-block mat block
			     (into #{} (for [y (range y1 y2) x (range x1 x2) :let [elem ((mat y) x)] :when (not (set? elem))] elem))))

(def blocks
     (concat (map #(vector 0 % 9 (+ % 1)) (range 9))
	     (map #(vector % 0 (+ % 1) 9) (range 9))
	     (for [x (range 0 9 3) y (range 0 9 3)] [x y (+ x 3) (+ y 3)])))

(defn parse-sudoku [mat]
  (reduce parse-block mat blocks))

(defn best-unsolved-square [mat]
  (let [unsolved-pairs (for [y (range 9) x (range 9) :let [elem ((mat y) x)] :when (set? elem)]
			 [[x y] elem])]
    (when-not (empty? unsolved-pairs)
      (ffirst (sort-by (comp count second) unsolved-pairs)))))

(defn solve [mat]
  (let [m (parse-sudoku mat)
	xy (best-unsolved-square m)]
    (if xy
      (mapcat #(solve (assoc-in m (reverse xy) %))
	      (get-in m (reverse xy)))
      [m])))

(def *sudoku1*
     (strings-to-matrix ["070801030"
			 "089000520"
			 "100209007"
			 "001504900"
			 "000000000"
			 "008703600"
			 "600405002"
			 "095000340"
			 "010306050"]))

(def *sudoku2*
     (strings-to-matrix ["032090080"
			 "005000000"
			 "000213000"
			 "009500030"
			 "640000051"
			 "050009400"
			 "000152000"
			 "000000700"
			 "090030510"]))

; TODO: should have this actually compare the results of solve to the correct answers
(defn run-tests []
  (print-matrices (concat [*sudoku1* *sudoku2* (parse-sudoku *sudoku1*) (parse-sudoku *sudoku2*)]
			  (solve *sudoku1*)
			  (solve *sudoku2*))))



