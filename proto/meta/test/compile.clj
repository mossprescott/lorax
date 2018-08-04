(ns meta.test.compile
  (:use (meta core)
        (meta.clojure kernel)))
        
(println
  (quote-node (make-node :foo { :bar (+ 1 2) })))

(def n 
  (make-node :kernel/quote {
    :body 
    (make-node :foo [
      (make-node :kernel/int { :value 1 })
      (make-node :kernel/int {
        :value
        (make-node :kernel/unquote {
          :body
          (make-node :kernel/int { :value 2 })
        })
      })
      (make-node :kernel/int {
        :value
        (make-node :kernel/unquote {
          :body
          (make-node :kernel/app {
            :expr
            (make-node :kernel/extern { :name "+" })
          
            :args 
            (make-node :kernel/args [
              (make-node :kernel/int { :value 3 })
              (make-node :kernel/int { :value 4 })
            ])
          })
        })
      })
    ])
  }))
  
(print-node n true)

(def c (meta-compile n))

(println "c:" c)

(print-node
  (eval c))
