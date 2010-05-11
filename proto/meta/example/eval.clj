;; This is just tests now

(ns meta.example.eval
  (:use (meta core reduce)
        (meta.clojure kernel core)))
  

(def n1 
  (node :clojure/core/and2 
    :left 
    "a"

    :right
    "b"))
(println "meta-reduce2:")
(print-node n1 true)
(let [ [n o] (meta-reduce2 n1 (reduceByType ccrules))]
  (print-node n true)
  (println o))

  
;;
;; Some simple tests...
;;
(defmacro run
  [n e]
  `(do
    (let [n# ~n]
      (println "Compiling:")
      (print-node n# "  ")
      (let [ c# (meta-compile n#) ]
        (println "Clojure:")
        (println (str "  " c#))
        (if ~e 
          (let [r# (meta-eval n#) ]
            (println "Result:" )
            (if (node? r#)
              (print-node r# "  ")
              (println (str "  " r#))))))
      (println))))

; (defn run nil) ;; HACK

(run
  (node :clojure/kernel/bind)
  false)
  
(run 
  (node :clojure/kernel/int 
    :value 1)
  false)

(run 
  (node :clojure/kernel/var
    :ref (node :core/ref :id :1))
  false)
                                  
(run 
  (node :clojure/kernel/app 
    :expr (node :clojure/kernel/extern :name '+)
    :args [ (node :clojure/kernel/int :value 1)
            (node :clojure/kernel/int :value 2)])
  true)

(run
  (node :clojure/kernel/let
    :bind (node :clojure/kernel/bind :core/id :1)
    :expr (node :clojure/kernel/int :value 2)
    :body 
      (node :clojure/kernel/var
        :ref (node :core/ref :id :1)))
  true)

(run
  (node :clojure/kernel/if
    :test (node :clojure/kernel/false)
    :then (node :clojure/kernel/int :value 1)
    :else (node :clojure/kernel/int :value 2))
  true)

(run
  (node :clojure/kernel/lambda
    :params [ (node :clojure/kernel/bind :core/id :1) ]
    :body 
    (node :clojure/kernel/var 
      :ref 
      (node :core/ref 
        :id :1)))
  false)

(run
  (node :clojure/kernel/app
    :expr
    (node :clojure/kernel/lambda
      :params [ (node :clojure/kernel/bind :core/id :1) ]
      :body 
      (node :clojure/kernel/var 
        :ref 
        (node :core/ref 
          :id :1)))
    :args [
      (node :clojure/kernel/int 
        :value 42) ])
  true)
  
(run 
  (node :core/later
    :node (node :clojure/kernel/int :value 1))
  true)

(def quoted
  (node :core/later
    :node
    (node :clojure/kernel/app 
      :expr (node :clojure/kernel/extern :name '+)
      :args [ 
        (node :clojure/kernel/int :value 1)
        (node :core/sooner 
          :node 
          (node :clojure/kernel/app 
            :expr (node :clojure/kernel/extern :name '+)
            :args [ 
              (node :clojure/kernel/int :value 2)
              (node :clojure/kernel/int :value 3)
            ]))
        ])))
    
(run 
  quoted
  true)

(run (meta-eval quoted) true)

(defmacro rrun
  [n e]
  `(do
    (println "Reduce:")
    (print-node ~n)
    (run (meta-reduce ~n (reduceByType ccrules)) ~e)))
    
(rrun
  (node :clojure/core/and2 
    :left 
    (node :clojure/kernel/true)
  
    :right 
    (node :clojure/kernel/false))
  true)
  
(rrun
  (node :clojure/core/and2-wrong 
    :left 
    (node :clojure/kernel/int
      :value 1)
  
    :right 
    (node :clojure/core/and2-wrong
      :left 
      (node :clojure/kernel/int
        :value 2)
  
      :right 
      (node :clojure/kernel/int
        :value 3)))
  true)
  
(rrun
  (node :clojure/core/and2 
    :left 
    (node :clojure/kernel/int
      :value 1)
  
    :right 
    (node :clojure/core/and2 
      :left 
      (node :clojure/kernel/int
        :value 2)
  
      :right 
      (node :clojure/kernel/int
        :value 3)))
  true)
