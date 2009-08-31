; Sudoku solver, implemented in Clojure
; By Vish Singh
; April 3, 2009

(ns sudoku
  (:use clojure.set))

; -----------------------------------------------------------------------------------------

(defn map2d [f mat]
  (doall (vec (map #(doall (vec (map f %))) mat))))

(defn map-n [f seq]
  (doall (vec (map f seq (iterate inc 0)))))

(defn map-n-cat [f seq]
  (doall (vec (mapcat f seq (iterate inc 0)))))

; -----------------------------------------------------------------------------------------

(defn strings-to-matrix-rep [strlist]
  (map2d (comp #(if (zero? %) (set (range 1 10)) %) #(- (int %) (int \0))) strlist))

(defn print-matrix [mat]
  (doseq [row mat] 
    (doseq [n row] 
      (if (set? n)
	(doseq [x (range 1 10)]
	  (if (n x) (printf "%d" x) (printf "_")))
	(printf "%9d" n))
      (printf " "))
    (newline)))

(defn print-matrices [mats]
  (doseq [mat mats]
    (print-matrix mat)
    (println (apply str (repeat 97 \-)))))

; -----------------------------------------------------------------------------------------

(defmacro union-map [[var seq] & body]
  `(apply union
	  (map (fn [~var] ~@body)
	       ~seq)))

(defn remove-numbers-from-block [mat x1 y1 x2 y2 numbers]
  (map-n (fn [row y]
	   (map-n (fn [elem x]
		    (if (and (>= x x1) (< x x2) (>= y y1) (< y y2) (set? elem))
		      (difference elem numbers)
		      elem))
		  row))
	 mat))

(defn parse-block [mat x1 y1 x2 y2]
  (remove-numbers-from-block mat x1 y1 x2 y2
			     (union-map [y (range y1 y2)]
			       (union-map [x (range x1 x2)]
				 (let [elem (get-in mat [y x])]
				   (if (set? elem) #{} #{elem}))))))

(defn parse-blocks [mat blocks]
  (if (empty? blocks)
    mat
    (recur (apply parse-block mat (first blocks)) (rest blocks))))

; -----------------------------------------------------------------------------------------

(defn parse-rows [mat]
  (parse-blocks mat (map #(vector 0 % 9 (+ % 1)) (range 9))))

(defn parse-columns [mat]
  (parse-blocks mat (map #(vector % 0 (+ % 1) 9) (range 9))))

(defn parse-squares [mat]
  (parse-blocks mat (for [x (range 0 9 3) y (range 0 9 3)] [x y (+ x 3) (+ y 3)])))

(def parse-sudoku
     (comp parse-rows parse-columns parse-squares))

; -----------------------------------------------------------------------------------------

(defn matrix-to-coord-pairs [mat]
  (map-n-cat (fn [row y] (map-n (fn [elem x] [[x y] elem])
				row))
	     mat))

(defn best-unsolved-square [mat]
  (let [unsolved-pairs (filter (comp set? second)
			       (matrix-to-coord-pairs mat))]
    (when-not (empty? unsolved-pairs)
      (first (first (sort-by (comp count second) unsolved-pairs))))))

; -----------------------------------------------------------------------------------------

(defn solve [mat]
  (let [m (parse-sudoku mat)
	xy (best-unsolved-square m)]
    (if xy
      (mapcat #(solve (assoc-in m (reverse xy) %))
	      (get-in m (reverse xy)))
      [m])))

; -----------------------------------------------------------------------------------------

(def *sudoku1*
     (strings-to-matrix-rep ["070801030"
			     "089000520"
			     "100209007"
			     "001504900"
			     "000000000"
			     "008703600"
			     "600405002"
			     "095000340"
			     "010306050"]))

(def *sudoku2*
     (strings-to-matrix-rep ["032090080"
			     "005000000"
			     "000213000"
			     "009500030"
			     "640000051"
			     "050009400"
			     "000152000"
			     "000000700"
			     "090030510"]))




