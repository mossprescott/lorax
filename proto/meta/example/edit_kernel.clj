(ns meta.example.edit-kernel
  (:use (clojure stacktrace)
        (meta.edit draw expr nodes)
        (meta check core reduce)
        (meta.clojure kernel)))

; (print-node
;   (parenthesize
;     (node :view/expr/binary
;       :boxes [
;         (node :view/expr/int :str "n")
;         (node :view/expr/symbol :str :times)
;         (node :view/expr/juxt
;           :boxes [
;             (node :view/expr/binary
;               :boxes [
;                 (node :view/expr/int :str "n")
;                 (node :view/expr/symbol :str "-")
;                 (node :view/expr/int :str "1")
;               ])
;             (node :view/expr/symbol :str "!")
;           ])
;       ])))

(def x1 
  (node :kernel/let 
    :bind 
    (node :kernel/bind :lorax/id :x)

    :expr 
    (node :kernel/int :value 1)

    :body
    (node :kernel/var 
      :ref
      (node :lorax/ref
        :id :x))))

(def x5
  (node :kernel/app
    :expr
    (node :kernel/extern
      :name "inc")
    
    :args [
      (node :kernel/int :value 1)
    ]))
    
; (def x2  ; identity fxn
;   (node :kernel/lambda
;     :params [
;       (node :kernel/bind :lorax/id :y)
;     ]
;     
;     :body
;     (node :kernel/var :ref (node :lorax/ref :id :y))))
; 
; (def x3
;   (node :kernel/app
;     :expr
;     x2
;     
;     :args [
;       (node :kernel/int :value 1)
;     ]))
    
(def x3
  (node :kernel/let
    :bind
    (node :kernel/bind :lorax/id :id)
    
    :expr
    (node :kernel/lambda
      :params [
        (node :kernel/bind :lorax/id :y)
      ]
      
      :body
      (node :kernel/var :ref (node :lorax/ref :id :y)))
    
    :body
    (node :kernel/app
      :expr
      (node :kernel/var :ref (node :lorax/ref :id :id))

      :args [
        (node :kernel/int :value 1)
      ])))

(def x4
  (node :kernel/let
    :bind
    (node :kernel/bind :lorax/id :z)
    
    :expr
    (node :kernel/lambda
      :params [
        (node :kernel/bind :lorax/id :t)
        (node :kernel/bind :lorax/id :u)
      ]
    
      :body
      (node :kernel/app
        :expr
        (node :kernel/extern :name "+")
        
        :args [
          (node :kernel/var :ref (node :lorax/ref :id :t))
          (node :kernel/var :ref (node :lorax/ref :id :u))
        ]))
      
    :body
    (node :kernel/app
      :expr
      (node :kernel/var :ref (node :lorax/ref :id :z))
      
      :args [
        (node :kernel/int :value 1)
        (node :kernel/int :value 2)
      ])))
      
(def x6
  (node :kernel/if
    :test
    (node :kernel/true)
    
    :then
    (node :kernel/int :value 1)
    
    :else
    (node :kernel/string :value "abc")))

(def x7
  (node :kernel/bind :lorax/id :w))

(def x8
  (node :kernel/let
    :bind
    (node :kernel/int :value 1)
    
    :expr
    (node :kernel/bind :lorax/id :q)
    
    :body
    (node :kernel/nil)))

(def x9
  (node :kernel/if
    :test
    (node :core/and
      :left
      (node :kernel/true)
    
      :right
      (node :kernel/app
        :expr
        (node :kernel/extern
          :name "=")
      
        :args [
          (node :kernel/int :value 1)
          (node :kernel/int :value 2)
        ]))
    
    :then
    (node :kernel/nil)
    
    :else
    (node :kernel/int :value 10000)))

(def x10
  (node :kernel/let
    :bind
    (node :kernel/bind :lorax/id :a)
    
    :expr
    (node :kernel/int :value 42)
    
    :body
    (node :kernel/quote
      :body
      (node :kernel/app
        :expr
        (node :kernel/extern
          :name "+")
        
        :args [
          (node :kernel/int :value 24)
          (node  :kernel/unquote
            :body
            (node :kernel/var
              :ref (node :lorax/ref :id :a)))
        ]))))
        
(def x11
  (node :kernel/if))

(def x12
  (node :kernel/app))

(def x13
  (node :kernel/lambda))

(def p3 
  (node :kernel/program
    ; :items [ x1 x2 x3 ]))
    :exprs [ x1 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 ]))

; (print-node (meta-reduce p3 kernelPresRules))

; (def struc (first (load-nodes "meta/clojure/kernel.mlj")))
; (def checker (make-structure-checker struc))
; (def errors (checker p3))

; (doseq [[k v] errors] (println k "->" (apply str (interpose "; " v))))

; (makeKernelFrame p3 "clojure/kernel" errors)

(let [cgr (load-node "meta/core.mlj")
      kgr1 (load-node "meta/clojure/kernel1.mlj")
      kgr2 (load-node "meta/clojure/kernel2.mlj")
      clgr (load-node "meta/clojure/core.mlj")
      gr (compose-grammars cgr kgr1 kgr2 clgr)
      struc (grammar-to-structure gr)

      checker (make-structure-checker struc)
      errors (checker p3)
      _ (println errors)

      displayStruc #(meta-reduce2 % (reduceByType structurePresRules))
      displayClojure #(meta-reduce2 % (grammar-to-display gr))
      ;[np o] (meta-reduce2 x5 display)
      ]
  ; (makeSyntaxFrame struc "clojure/kernel example (grammar -> structure)" displayStruc {})
  ; (print-node np true)
  (makeSyntaxFrame p3 "clojure/kernel example (grammar -> display)" displayClojure {})
  )
  
  