(defmacro mytime
  "Evaluates expr and returns the time it took (in ms) and the value of expr."
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     [(/ (double (- (. System (nanoTime)) start#)) 1000000.0) ret#]))

(defn t2 
	"(t2 ['f1 'f2] times arg)"
	[fs times x]
	(let [expected ((eval (first fs)) x)
		  avgs (map #(/ % times)
    					(loop [i 0 
    							sums (take (count fs) (repeat 0))]
    						(let [s (map #((mytime ((eval %) x)) 0) fs)]
    							(if (< i times)
    								(recur (inc i) (map + sums s))
    								sums))))]
    ; (doall (map prn avgs))
		(for [i (range (count fs))] [(fs i) (nth avgs i)])))



(defn sum [n] 
	(let [v (loop [i 0 acc []]
				(if (< i n)
					(recur (inc i) (conj acc i))
					acc))]
		(reduce + v)))

(defn sum2 [n] 
	(let [v (loop [i 0 acc (transient [])]
				(if (< i n)
					(recur (inc i) (conj! acc i))
					acc))
		pv (persistent! v)]
		(reduce + pv)))
		
(defn vrange [n]
  (loop [i 0 v []]
    (if (< i n)
      (recur (inc i) (conj v i))
      v)))
 
(defn vrange2 [n]
  (loop [i 0 v (transient [])]
    (if (< i n)
      (recur (inc i) (conj! v i))
      (persistent! v))))

(defn zeros [n]
  (loop [i 0 v []]
    (if (< i n)
      (recur (inc i) (conj v 0))
      v)))
 
(defn zeros2 [n]
  (loop [i 0 v (transient [])]
    (if (< i n)
      (recur (inc i) (conj! v 0))
      (persistent! v))))

(defn copies [x n]
  (loop [i 0 v []]
    (if (< i n)
      (recur (inc i) (conj v x))
      v)))
 
(defn copies2 [x n]
  (loop [i 0 v (transient [])]
    (if (< i n)
      (recur (inc i) (conj! v x))
      (persistent! v))))

 
; (defn enumerate 
;   "(strict) list of pairs (index elem) from s"
;   [s]
;   (let [e (fn [i s]
;             (if (nil? s)
;             ()
;             (conj (list i (first s)) 
;                   (recur (inc i) (rest s)))))] ; Cannot recur! Not tail position!
;         (e 0 s)))
	
	
(defmacro und
  ([x y]
    `(let [x# ~x]
      (if (not x#) x# ~y))))

; (defmacro unq 
;   ([x] 
;     '(~(x))))

(defmacro und2
  ([x y]
    '(let [x1 ~x]
      (if x1 ~y ~x))))

(defmacro plus
  "Evaluates its arg twice, then adds the result."
  ([x]
    `(+ ~x ~x)))

(defmacro plusser
  "Returns a fn that evaluates its (the macro's) arg twice, then adds the result."
  ([x]
    `(fn [] (+ ~x ~x))))

;(print (t2 [