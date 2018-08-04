(defmacro simple-for
  [[x xs] expr]
  `((fn f# [s#]
      (if (seq s#)
        (cons
          (let [~x (first s#)] ~expr)
          (lazy-seq (f# (rest s#))))))
      ~xs))
      
(println (simple-for [x (range 1 11)] (* x x)))

(defmacro simple-for2
  [[x xs] expr]
  `((fn f# [s#]
      (if (seq s#)
        (let [[~x & r#] s#]
          (cons ~expr
            (lazy-seq (f# r#))))))
      ~xs))
      
(println (simple-for2 [x (range 1 11)] (* x x)))

