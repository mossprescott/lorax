; This file will contain definitions pertaining to the kernel language, and 
; perhaps the translation from the kernel language to ordinary Clojure forms.

(ns meta.clojure.kernel
  (:use (clojure test)
        (meta core reduce)))
  


;
; Compilation from nodes of :clojure/kernel language to ordinary Clojure forms:
;

(def meta-compile-later)  ;; forward declaration!
(def meta-eval)

(defn meta-compile
  "Given a node satisfying the grammar for Clojure kernel programs, return
  the forms of the equivalent ordinary Clojure program."
  [n]
  (let [ symbolFromId #(symbol (str "_" (subs (str %) 1))) ]
    (condp = (node-type n)
      :clojure/kernel/bind
      (symbolFromId (node-id n))
        
      :clojure/kernel/lambda
      `(fn ~(symbolFromId (node-id n))
          [ ~@(map meta-compile (node-attr n :clojure/kernel/lambda/params)) ]
          ~(meta-compile (node-attr n :clojure/kernel/lambda/body)))
        
      :clojure/kernel/app
      `(~(meta-compile (node-attr n :clojure/kernel/app/expr)) 
        ~@(map meta-compile (node-attr n :clojure/kernel/app/args)))
          
      :clojure/kernel/if
      `(if ~(meta-compile (node-attr n :clojure/kernel/if/test))
        ~(meta-compile (node-attr n :clojure/kernel/if/then))
        ~(meta-compile (node-attr n :clojure/kernel/if/else)))

      :clojure/kernel/let
      `(let [ ~(symbolFromId (-> n (node-attr :clojure/kernel/let/bind) node-id))
                ~(meta-compile (node-attr n :clojure/kernel/let/expr)) ]
        ~(meta-compile (node-attr n :clojure/kernel/let/body)))
         
      :clojure/kernel/var
      (symbolFromId (-> n (node-attr :clojure/kernel/var/ref) (node-attr :core/ref/id)))
        
      :clojure/kernel/true true
      :clojure/kernel/false false
      :clojure/kernel/nil nil
        
      :clojure/kernel/int
      (node-attr n :value)
        
      :clojure/kernel/string
      (node-attr n :value)
        
      :clojure/kernel/extern
      (symbol (node-attr n :name))

      ; t :core/later
      ;        ; Note: nodes are represented as maps, and therefore implicitly quoted
      ;        ; at the Clojure level.
      ;        ; TODO: need syntax quoting here, so embedded unquotes will be evaluated
      ;        (meta-compile-later (node-attr n :core/later/node))

      :clojure/kernel/quote
      ; Note: nodes are represented as maps, and therefore implicitly quoted
      ; at the Clojure level.
      ; TODO: need syntax quoting here, so embedded unquotes will be evaluated
      (meta-compile-later (node-attr n :clojure/kernel/quote/body))
      
;      (= t :core/sooner) ; should never be encountered here?
      
      (do 
        (println "unrecognized node type:" (node-type n))
        (assert false)))))


(defn- meta-compile-later
  [n]
  (cond
    (node? n)
    (if (= (node-type n) :clojure/kernel/unquote)
      ; handle "sooner" node now:
      (meta-compile (node-attr n :body))  ; what if the result is a raw value? 
                                          ; how does it get wrapped in nodes?
    
      ; recursively visit the children:
      (zipmap 
        (keys n) 
        (for [k (keys n)] 
          (meta-compile-later (node-attr n k)))))
      
    (vector? n)
    (vec (map meta-compile-later n))
    
    true
    n))

(defn meta-eval
  "Given a program in Clojure kernel syntax, compile it to raw Clojure forms,
  evaluate them, and wrap the result in syntax."
  [n]
  (let [ ; _ (print-node n true)
        c (meta-compile n)
        ; _ (println c)
        r (eval c) ]
    (cond
      (node? r)
      r
      
      (nil? r)
      (node :clojure/kernel/nil)
      
      (= true r)
      (node :clojure/kernel/true)
      
      (= false r)
      (node :clojure/kernel/false)
      
      (integer? r)
      (node :clojure/kernel/int 
        :value r)
        
      (string? r)
      (node :clojure/kernel/string
        :value r))))


;
; Reduction rules for presentation:
;
(defn nameFromId 
  "Take a node id (a keyword) and return a vaguely human-readable variable name,
  currently 'x---'. This is good enough to make programs make sense, but obviously
  something better will be needed eventually."
  [id]
  ; (str "x" (subs (str id) 1)))
  (subs (str id) 1))


; Hand-written reduction for the :clojure/kernel language; supplanted by the
; grammar in kernel2.mlj
(def kernelPresRules {
  :clojure/kernel/bind
  (fn [n] 
    (node :view/expr/var :str (nameFromId (node-id n))))
    
  :clojure/kernel/var
  (fn [n] 
    (let [refNode (node-attr n :clojure/kernel/var/ref)  ; TODO: handle missing ref
          refId (node-attr refNode :core/ref/id)]         ; TODO: handle missing id?
      (node :view/expr/var :str (nameFromId refId))))
      
  ; ugly generic syntax with keywords "function" and ???:
  :clojure/kernel/lambda
  (fn [n]
    (node :view/expr/flow
      :boxes [
        (node :view/expr/keyword :str "function")
        (node :view/expr/juxt
          :boxes
          (vec (interpose 
                  (node :view/sequence
                    :items [
                      (node :view/expr/keyword :str ",")
                      (node :view/thinspace)
                    ])
                  (with-attr-seq n :clojure/kernel/lambda/params))))
        (node :view/expr/symbol :str :to)
        (with-attr-node n :clojure/kernel/lambda/body)
      ]))

  ; Lambda-calculus-style syntax with lambda and ".":
  ; :clojure/kernel/lambda
  ; (fn [n]
  ;   (node :view/expr/binary
  ;     :boxes [
  ;       ; (node :view/expr/keyword :str "lambda")
  ;       (node :view/expr/symbol :str :lambda) ; HACK?
  ;       (node :view/expr/juxt
  ;         :boxes
  ;         (vec (interpose 
  ;                 (node :view/sequence
  ;                   :items [
  ;                     (node :view/expr/keyword :str ",")
  ;                     (node :view/thinspace)
  ;                   ])
  ;                 (with-attr-seq n :clojure/kernel/lambda/params))))
  ;       (node :view/expr/keyword :str ".")
  ;       (with-attr-node n :clojure/kernel/lambda/body)
  ;     ]))
  ; 
  :clojure/kernel/app  ; ugly generic syntax with the keywords "apply" and "to"
  (fn [n]
    (node :view/expr/flow
      :boxes [
        (node :view/expr/keyword :str "apply")
        (with-attr-node n :clojure/kernel/app/expr)
        (node :view/expr/keyword :str "to")
        (node :view/expr/juxt
          :boxes 
          (vec (interpose 
                  (node :view/sequence
                    :items [
                      (node :view/expr/keyword :str ",")
                      (node :view/thinspace)
                    ])
                    (with-attr-seq n :clojure/kernel/app/args))))
      ]))
      
  ; :clojure/kernel/app  ; "Haskell"-style application by juxtaposition (here, with medium space)
  ; (fn [n]
  ;   (node :view/expr/relation
  ;     :boxes
  ;     (vec (cons (with-attr-node n :clojure/kernel/app/expr) 
  ;               (with-attr-seq n :clojure/kernel/app/args)))
  ;     ))
      
  ; :clojure/kernel/app  ; "C"-style application with parens
  ; (fn [n]
  ;   (node :view/expr/juxt
  ;     :boxes [
  ;       (node-attr n :clojure/kernel/app/expr) 
  ;       (node :view/parens
  ;         :left "(" :right ")"
  ;         :content 
  ;           (first (node-attr n :clojure/kernel/app/args)))  ; HACK: one arg only!
  ;     ]))
      
  :clojure/kernel/int
  (fn [n]
    (with-attr-node n :clojure/kernel/int/value v
      (node :view/expr/int 
        :str 
        (str v))))

  :clojure/kernel/string
  (fn [n]
    (with-attr-node n :clojure/kernel/string/value v
      (node :view/expr/string
        :str 
        (str v))))
  
  :clojure/kernel/true
  (fn [n]
    (node :view/expr/keyword 
      :str "true"))
  
  :clojure/kernel/false
  (fn [n]
    (node :view/expr/keyword 
      :str "false"))
  
  :clojure/kernel/nil
  (fn [n]
    (node :view/expr/keyword 
      :str "nil"))
  
  :clojure/kernel/let
  (fn [n]
    (node :view/section
      :items [
        (node :view/expr/flow
          :boxes [
            (node :view/expr/keyword :str "let")
            (with-attr-node n :clojure/kernel/let/bind)
            (node :view/expr/symbol :str "=")
            (with-attr-node n :clojure/kernel/let/expr)
          ])
        (node :view/sequence
          :items [
            (node :view/quad)
            (node :view/expr/flow
              :boxes [
                (node :view/expr/keyword :str "in")
                (with-attr-node n :clojure/kernel/let/body)
              ])
          ])
      ]))
      
  :clojure/kernel/if
  (fn [n]
    (node :view/section
      :items [
        (node :view/expr/flow
          :boxes [
            (node :view/expr/keyword :str "if")
            (with-attr-node n :clojure/kernel/if/test)
          ])
        (node :view/sequence
          :items [
          (node :view/quad)
          (node :view/expr/flow
            :boxes [
              (node :view/expr/keyword :str "then")
              (with-attr-node n :clojure/kernel/if/then)
            ])
          ])
        (node :view/sequence
          :items [
          (node :view/quad)
          (node :view/expr/flow
            :boxes [
              (node :view/expr/keyword :str "else")
              (with-attr-node n :clojure/kernel/if/else)
            ])
          ])
      ]))
      
  :clojure/kernel/extern
  (fn [n]
    (with-attr-node n :clojure/kernel/extern/name nm
      (node :view/expr/mono
        :str
        (str "\"" nm "\""))))

  ; ; Backtick:
  ; :core/later
  ; (fn [n]
  ;   (node :view/expr/juxt
  ;     :boxes [
  ;       (node :view/chars :str "\u00d2" :font :cmr10)
  ;       (node-attr n :core/later/node)
  ;     ]))

  ; Bordered:
  ; :core/later
  ; (fn [n]
  ;   (node :view/border
  ;     :weight 2
  ;     :margin 0
  ;     :view/drawable/colors [
  ;       (node :view/gray :brightness 0.7)
  ;     ]
  ;       
  ;     :item
  ;     (node :view/border
  ;       :weight 2
  ;       :margin 2
  ;       :view/drawable/colors [
  ;         (node :view/gray :brightness 0.3)
  ;       ]
  ;       
  ;       :item 
  ;       (with-attr-node n :core/later/node))))

  ; Beveled:
  :core/later
  (fn [n]
    (node :view/border
      :weight 1
      :margin 2
      :view/drawable/colors [
        (node :view/gray :brightness 0.5)
        (node :view/gray :brightness 0.9)
      ]
      
      :item 
      (with-attr-node n :core/later/node)))

  ; Tilde:
  ; :core/sooner
  ; (fn [n]
  ;   (node :view/expr/juxt
  ;     :boxes [
  ;       (node :view/chars :str "\u007e" :font :cmr10)
  ;       (node-attr n :core/sooner/node)
  ;     ]))

  ; Bordered:
  ; :core/sooner
  ; (fn [n]
  ;   (node :view/border
  ;     :weight 2
  ;     :margin 0
  ;     :view/drawable/colors [
  ;       (node :view/gray :brightness 0.3)
  ;     ]
  ; 
  ;     :item
  ;     (node :view/border
  ;       :weight 2
  ;       :margin 2
  ;       :view/drawable/colors [
  ;         (node :view/gray :brightness 0.7)
  ;       ]
  ;       
  ;       :item 
  ;       (with-attr-node n :core/sooner/node))))

  ; Beveled:
  :core/sooner
  (fn [n]
    (node :view/border
      :weight 1
      :margin 2
      :view/drawable/colors [
        (node :view/gray :brightness 0.9)
        (node :view/gray :brightness 0.5)
      ]
      
      :item 
      (with-attr-node n :core/sooner/node)))
      

  ; HACK: not really a kernel language construct
  :clojure/kernel/program
  (fn [n]
    (node :view/section
      :items 
      (vec (interpose
        (node :view/chars :str " " :font :cmr10)  ; HACK: a blank line, effectively
        (with-attr-seq n :clojure/kernel/program/exprs)))))
})


;
; Tests:
;

(deftest compile1
  (is (= 1
        (meta-compile 
          (node :clojure/kernel/int 
            :value 1))))
  (is (= true
        (meta-compile 
          (node :clojure/kernel/true))))
  (is (= "abc"
        (meta-compile 
          (node :clojure/kernel/string
            :value "abc")))))

(deftest eval1
  (is (= 3
        (eval (meta-compile
          ; (println (meta-compile
          (node :clojure/kernel/app
            :expr
            (node :clojure/kernel/extern :name "+")
            
            :args [
              (node :clojure/kernel/int :value 1)
              (node :clojure/kernel/int :value 2)
            ])))))
  (is (= 1
        (eval (meta-compile
          (node :clojure/kernel/let
            :bind
            (node :clojure/kernel/bind :core/id :x)
            
            :expr
            (node :clojure/kernel/int :value 1)
            
            :body
            (node :clojure/kernel/var
              :ref (ref-node :x))))))))
                
; (deftest quote1
;   (let [n (node :clojure/kernel/let
;             :bind
;             (node :clojure/kernel/bind :core/id :x)
;             
;             :expr
;             (node :clojure/kernel/int :value 1)
;             
;             :body
;             (node :clojure/kernel/quote
;               :body
;               (node :clojure/kernel/app
;                 :expr
;                 (node :clojure/kernel/extern
;                   :name "+")
;                   
;                 :args [
;                   (node :clojure/kernel/unquote
;                     :body
;                     (node :clojure/kernel/var
;                       :ref (ref-node :x)))
;                   (node :clojure/kernel/int
;                     :value 2)
;                 ])))
;           _ (println (meta-compile n))
;           _ (print-node (meta-eval n) true)]
;     (is (= 1
;           (meta-compile (meta-eval n))))))
                