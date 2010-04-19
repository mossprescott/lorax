(defn funny-named-fxn? []
	nil)

(print "Hello, world\n")

(def a 1)

a

(print "\n")

"Clojure is wierd"

; (defmacro addTo [x]
;     '(fn [y] (+ ~x y)))
;     
; (def myinc (addTo 1))
; 
; myinc
; 
; (myinc 3)


;(doc doc)

(defn s [x]
	"Loop without any type hint"
	(loop [i 0 acc 0]
		(if (< i x)
			(recur (inc i) (+ acc i))
			acc)))
			
(comment "Loop with cast to int (~10% slower)")
(defn s2 [x]
	(def x2 (int x))
	(loop [i 0 acc 0]
		(if (< i x2)
			(recur (inc i) (+ acc i))
			acc)))

(comment "Simple reduce (30% faster)")
(defn s3 [x]
	(reduce + (range x)))

(comment "Reduce with trivial closure (same as s3)")
(defn s4 [x]
	(reduce #(+ %1 %2) (range x)))

(defmacro mytime
  "Evaluates expr and returns the time it took (in ms) and the value of expr."
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     [(/ (double (- (. System (nanoTime)) start#)) 1000000.0) ret#]))


(defn t [x]
	(time (s x))
	(time (s2 x))
	(time (s3 x))
	(time (s4 x))
	nil)

(defn t2 [fs times x]
	(let [expected ((first fs) x)
		  avgs (map #(/ % times)
					(loop [i 0 
							sums (take (count fs) (repeat 0))]
						(let [s (map #((mytime (% x)) 0) fs)]
							(comment (prn s))
							(comment (prn sums))
							(if (< i times)
								(recur (inc i) (map + sums s))
								sums))))]
		(map prn avgs)
		avgs))

; (comment "Run each version a few times to get hopefully fair comparison")
; '(dotimes [i 10] 
; 	(t 1000000)
; 	(print "\n"))

; (map prn (t2 [s s2 s3 s4] 10 1000000))
; (prn (t2 [s s2 s3 s4] 10 1000000))

(def pt (for [x (range 1 1000) 
				y (range x 1000) 
				z (range y 1000);(+ (* x x) (* y y) 1)) 
				:when (= (+ (* x x) (* y y)) (* z z))] 
			[ x y z ]))

(defn lazy-print [s]
	(loop [i 1 s s]
		(if (seq s)
			(do (prn i (first s))
				(recur (inc i) (rest s)))
			nil)))

; (prn (myime (count pt)))
; (lazy-print pt)
; (time (lazy-print (take 100 pt)))

(defn fact [n]
	(if (> n 1)
		(* n (fact (dec n)))
		1))

(defn sumto [n]
	(if (> n 0)
		(+ n (sumto (dec n)))
		0))

(defn sumto2 [n]
	(loop [i 0 acc 0]
		(if (<= i n)
			(recur (inc i) (+ acc i))
			acc)))

; (defn gcd [a b]
	