;; This is just tests now

(ns meta.example.eval
  (:use (meta core reduce)
        (meta.clojure kernel core)))
  

(def n1 
  (node :core/and2 
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
  (node :kernel/bind)
  false)
  
(run 
  (node :kernel/int 
    :value 1)
  false)

(run 
  (node :kernel/var
    :ref (node :lorax/ref :id :1))
  false)
                                  
(run 
  (node :kernel/app 
    :expr (node :kernel/extern :name '+)
    :args [ (node :kernel/int :value 1)
            (node :kernel/int :value 2)])
  true)

(run
  (node :kernel/let
    :bind (node :kernel/bind :lorax/id :1)
    :expr (node :kernel/int :value 2)
    :body 
      (node :kernel/var
        :ref (node :lorax/ref :id :1)))
  true)

(run
  (node :kernel/if
    :test (node :kernel/false)
    :then (node :kernel/int :value 1)
    :else (node :kernel/int :value 2))
  true)

(run
  (node :kernel/lambda
    :params [ (node :kernel/bind :lorax/id :1) ]
    :body 
    (node :kernel/var 
      :ref 
      (node :lorax/ref 
        :id :1)))
  false)

(run
  (node :kernel/app
    :expr
    (node :kernel/lambda
      :params [ (node :kernel/bind :lorax/id :1) ]
      :body 
      (node :kernel/var 
        :ref 
        (node :lorax/ref 
          :id :1)))
    :args [
      (node :kernel/int 
        :value 42) ])
  true)
  
(run 
  (node :lorax/later
    :node (node :kernel/int :value 1))
  true)

(def quoted
  (node :lorax/later
    :node
    (node :kernel/app 
      :expr (node :kernel/extern :name '+)
      :args [ 
        (node :kernel/int :value 1)
        (node :lorax/sooner 
          :node 
          (node :kernel/app 
            :expr (node :kernel/extern :name '+)
            :args [ 
              (node :kernel/int :value 2)
              (node :kernel/int :value 3)
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
  (node :core/and2 
    :left 
    (node :kernel/true)
  
    :right 
    (node :kernel/false))
  true)
  
(rrun
  (node :core/and2-wrong 
    :left 
    (node :kernel/int
      :value 1)
  
    :right 
    (node :core/and2-wrong
      :left 
      (node :kernel/int
        :value 2)
  
      :right 
      (node :kernel/int
        :value 3)))
  true)
  
(rrun
  (node :core/and2 
    :left 
    (node :kernel/int
      :value 1)
  
    :right 
    (node :core/and2 
      :left 
      (node :kernel/int
        :value 2)
  
      :right 
      (node :kernel/int
        :value 3)))
  true)
