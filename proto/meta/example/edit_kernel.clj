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
  (node :clojure/kernel/let 
    :bind 
    (node :clojure/kernel/bind :core/id :x)

    :expr 
    (node :clojure/kernel/int :value 1)

    :body
    (node :clojure/kernel/var 
      :ref
      (node :core/ref
        :id :x))))

(def x5
  (node :clojure/kernel/app
    :expr
    (node :clojure/kernel/extern
      :name "inc")
    
    :args [
      (node :clojure/kernel/int :value 1)
    ]))
    
; (def x2  ; identity fxn
;   (node :clojure/kernel/lambda
;     :params [
;       (node :clojure/kernel/bind :core/id :y)
;     ]
;     
;     :body
;     (node :clojure/kernel/var :ref (node :core/ref :id :y))))
; 
; (def x3
;   (node :clojure/kernel/app
;     :expr
;     x2
;     
;     :args [
;       (node :clojure/kernel/int :value 1)
;     ]))
    
(def x3
  (node :clojure/kernel/let
    :bind
    (node :clojure/kernel/bind :core/id :id)
    
    :expr
    (node :clojure/kernel/lambda
      :params [
        (node :clojure/kernel/bind :core/id :y)
      ]
      
      :body
      (node :clojure/kernel/var :ref (node :core/ref :id :y)))
    
    :body
    (node :clojure/kernel/app
      :expr
      (node :clojure/kernel/var :ref (node :core/ref :id :id))

      :args [
        (node :clojure/kernel/int :value 1)
      ])))

(def x4
  (node :clojure/kernel/let
    :bind
    (node :clojure/kernel/bind :core/id :z)
    
    :expr
    (node :clojure/kernel/lambda
      :params [
        (node :clojure/kernel/bind :core/id :t)
        (node :clojure/kernel/bind :core/id :u)
      ]
    
      :body
      (node :clojure/kernel/app
        :expr
        (node :clojure/kernel/extern :name "+")
        
        :args [
          (node :clojure/kernel/var :ref (node :core/ref :id :t))
          (node :clojure/kernel/var :ref (node :core/ref :id :u))
        ]))
      
    :body
    (node :clojure/kernel/app
      :expr
      (node :clojure/kernel/var :ref (node :core/ref :id :z))
      
      :args [
        (node :clojure/kernel/int :value 1)
        (node :clojure/kernel/int :value 2)
      ])))
      
(def x6
  (node :clojure/kernel/if
    :test
    (node :clojure/kernel/true)
    
    :then
    (node :clojure/kernel/int :value 1)
    
    :else
    (node :clojure/kernel/string :value "abc")))

(def x7
  (node :clojure/kernel/bind :core/id :w))

(def x8
  (node :clojure/kernel/let
    :bind
    (node :clojure/kernel/int :value 1)
    
    :expr
    (node :clojure/kernel/bind :core/id :q)
    
    :body
    (node :clojure/kernel/nil)))

(def x9
  (node :clojure/kernel/if
    :test
    (node :clojure/core/and
      :left
      (node :clojure/kernel/true)
    
      :right
      (node :clojure/kernel/app
        :expr
        (node :clojure/kernel/extern
          :name "=")
      
        :args [
          (node :clojure/kernel/int :value 1)
          (node :clojure/kernel/int :value 2)
        ]))
    
    :then
    (node :clojure/kernel/nil)
    
    :else
    (node :clojure/kernel/int :value 10000)))

(def x10
  (node :clojure/kernel/let
    :bind
    (node :clojure/kernel/bind :core/id :a)
    
    :expr
    (node :clojure/kernel/int :value 42)
    
    :body
    (node :clojure/kernel/quote
      :body
      (node :clojure/kernel/app
        :expr
        (node :clojure/kernel/extern
          :name "+")
        
        :args [
          (node :clojure/kernel/int :value 24)
          (node  :clojure/kernel/unquote
            :body
            (node :clojure/kernel/var
              :ref (node :core/ref :id :a)))
        ]))))
        
(def x11
  (node :clojure/kernel/if))

(def x12
  (node :clojure/kernel/app))

(def x13
  (node :clojure/kernel/lambda))

(def p3 
  (node :clojure/kernel/program
    ; :items [ x1 x2 x3 ]))
    :exprs [ x1 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 ]))

; (print-node (meta-reduce p3 kernelPresRules))

; (def struc (first (load-nodes "meta/clojure/kernel.mlj")))
; (def checker (make-structure-checker struc))
; (def errors (checker p3))

; (doseq [[k v] errors] (println k "->" (apply str (interpose "; " v))))

; (makeKernelFrame p3 "clojure/kernel" errors)

(let [cgr (load-node "meta/core.mlj")
      kgr (load-node "meta/clojure/kernel.mlj")
      clgr (load-node "meta/clojure/core.mlj")
      gr (compose-grammars cgr kgr clgr)
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
  
  