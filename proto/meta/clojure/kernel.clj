; This file will contain definitions pertaining to the kernel language, and 
; perhaps the translation from the kernel language to ordinary Clojure forms.

(ns meta.clojure.kernel
  (:use (meta core reduce)))
  
(def meta-compile-later)  ;; forward declaration!
(def meta-eval)

(defn meta-compile
  "Given a node satisfying the grammar for Clojure kernel programs, return
  the forms of the equivalent ordinary Clojure program."
  [n]
  (let [ symbolFromId #(symbol (str "_" (subs (str %) 1))) 
          t (node-type n) ]
    (cond 
      (= t :clojure/kernel/bind)
        (symbolFromId (node-id n))
        
      (= t :clojure/kernel/lambda)
        `(fn ~(symbolFromId (node-id n))
            [ ~@(map meta-compile (node-attr n :clojure/kernel/lambda/params)) ]
            ~(meta-compile (node-attr n :clojure/kernel/lambda/body)))
        
      (= t :clojure/kernel/app)
        `(~(meta-compile (node-attr n :clojure/kernel/app/expr)) 
          ~@(map meta-compile (node-attr n :clojure/kernel/app/args)))
          
      (= t :clojure/kernel/if) 
        `(if ~(meta-compile (node-attr n :clojure/kernel/if/test))
          ~(meta-compile (node-attr n :clojure/kernel/if/then))
          ~(meta-compile (node-attr n :clojure/kernel/if/else)))

      (= t :clojure/kernel/let) 
         `(let [ ~(symbolFromId (-> n (node-attr :clojure/kernel/let/bind) node-id))
                    ~(meta-compile (node-attr n :clojure/kernel/let/expr)) ]
            ~(meta-compile (node-attr n :clojure/kernel/let/body)))
         
      (= t :clojure/kernel/var) 
        (symbolFromId (-> n (node-attr :clojure/kernel/var/ref) (node-attr :core/ref/id)))
        
      (= t :clojure/kernel/true) true
      (= t :clojure/kernel/false) false
        
      (= t :clojure/kernel/int) 
        (n :clojure/kernel/int/value)
        
      (= t :clojure/kernel/extern) 
        (n :clojure/kernel/extern/name)

      (= t :core/later) 
        ; Note: nodes are represented as maps, and therefore implicitly quoted
        ; at the Clojure level.
        ; TODO: need syntax quoting here, so embedded unquotes will be evaluated
        (meta-compile-later (node-attr n :core/later/node))
      
;      (= t :core/sooner) ; should never be encountered here?
      
      true
        (do 
          (println "unrecognized node type:" t)
          (assert false)))))


(defn- meta-compile-later
  [n]
  (cond
    (node? n)
    (if (= (node-type n) :core/sooner)
      ; handle "sooner" node now:
      (meta-eval (node-attr n :core/sooner/node))
    
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
  (let [ c (meta-compile n)
        r (eval c) ]
    (cond
      (node? r)
      r
      
      (= true r)
      (node :/clojure/kernel/true)
      
      (= false r)
      (node :/clojure/kernel/false)
      
      (integer? r)
      (node :clojure/kernel/int 
        :value r))))


;
; Reduction rules for presentation:
;
(defn nameFromId 
  "Take a node id (a keyword) and produces a vaguely human-readable variable name,
  currently 'x---'. This is good enough to make programs make sense, but obviously
  something better will be needed eventually."
  [id]
  ; (str "x" (subs (str id) 1)))
  (subs (str id) 1))

; TODO: move these rules to a .mlj file, using the new pattern-matching reduction syntax...
(def kernelPresRules {
  :clojure/kernel/bind
  (fn [n] 
    (node :view/expr/var :str (nameFromId (node-id n))))
    
  :clojure/kernel/var
  (fn [n] 
    (let [refNode (node-attr n :clojure/kernel/var/ref)  ; TODO: handle missing ref
          refId (node-attr refNode :core/ref/id)]         ; TODO: handle missing id?
      (node :view/expr/var :str (nameFromId refId))))

  :clojure/kernel/lambda
  (fn [n]
    (node :view/expr/binary
      :boxes [
        ; (node :view/expr/keyword :str "lambda")
        (node :view/expr/symbol :str :lambda) ; HACK?
        (node :view/expr/juxt
          :boxes
          (vec (interpose 
                  (node :view/sequence
                    :items [
                      (node :view/expr/keyword :str ",")
                      (node :view/thinspace)
                    ])
                  (with-attr-seq n :clojure/kernel/lambda/params))))
        (node :view/expr/keyword :str ".")
        (with-attr-node n :clojure/kernel/lambda/body)
      ]))

  :clojure/kernel/app  ; "Haskell"-style application by juxtaposition (here, with medium space)
  (fn [n]
    (node :view/expr/relation
      :boxes
      (vec (cons (with-attr-node n :clojure/kernel/app/expr) 
                (with-attr-seq n :clojure/kernel/app/args)))
      ))
      
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
    (with-attr-node n :clojure/kernel/extern/name n
      (node :view/expr/mono
        :str
        (str "\"" n "\""))))

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
