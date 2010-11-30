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

(defn quote-node
  "Turns a node or value into clojure forms which, when evaluated, produce the 
  same node.
  When nodes were represented as maps, this was a no-op. Now that there's a 
  non-Clojure entity there, this is more painful. On top of that, it's 
  inefficient to take them apart and then put them back together. An 
  alternative would be to somehow bind them to names?"
  [n]
  (if-not (node? n)
    n
    (let [val (cond
                (map-node? n)
                (mapfor [a (node-attrs n)] a (quote-node (node-attr n a)))
    
                (seq-node? n)
                (vec (for [c (node-children n)] (quote-node c)))
                
                true
                (node-value n))]
      `(make-node ~(node-type n)
                  ~(node-id n)
                  ~val))))

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
          [ ~@(map meta-compile (node-children (node-attr n :params))) ]
          ~(meta-compile (node-attr n :body)))
        
      :clojure/kernel/recur
      `(recur
        ~@(map meta-compile (node-children (node-attr n :args))))
      
      :clojure/kernel/app
      `(~(meta-compile (node-attr n :expr)) 
        ~@(map meta-compile (node-children (node-attr n :args))))
          
      :clojure/kernel/if
      `(if ~(meta-compile (node-attr n :test))
        ~(meta-compile (node-attr n :then))
        ~(meta-compile (node-attr n :else)))

      :clojure/kernel/let
      ; (let [q  ; HACK
      `(let [ ~(symbolFromId (-> n (node-attr :bind) node-id))
                ~(meta-compile (node-attr n :expr)) ]
        ~(meta-compile (node-attr n :body)))
      ; ] (do (println "let:" q) q))  ; HACK
         
      :clojure/kernel/var
      ; (do (println "var:" n)  ; HACK
      (symbolFromId (-> n (node-attr :ref) ref-node-id))
      ; ) ; HACK
        
      :clojure/kernel/true true
      :clojure/kernel/false false
      :clojure/kernel/nil nil
        
      :clojure/kernel/int
      (node-attr-value n :value)
        
      :clojure/kernel/string
      (node-attr-value n :value)
        
      :clojure/kernel/extern
      ; (do (println "extern:" n)  ; HACK
      (symbol (node-attr-value n :name))
      ; ) ; HACK
      
      :clojure/kernel/name
      (node-attr-value n :value)
      
      ; t :core/later
      ;        ; Note: nodes are represented as maps, and therefore implicitly quoted
      ;        ; at the Clojure level.
      ;        ; TODO: need syntax quoting here, so embedded unquotes will be evaluated
      ;        (meta-compile-later (node-attr n :core/later/node))

      :clojure/kernel/quote
      (meta-compile-later (node-attr n :body) 1)
      
;      :clojure/kernel/unquote ; should never be encountered here?
      
      (do 
        (println "unrecognized node type (not in kernel language):" (node-type n))
        (print-node n)  ; HACK
        (assert false)))))


(defn- meta-compile-later
  "level - initially 1. Incremented when another quote is seen, and decremented
      on unquote, until it gets back to 1, and then we go back to normal compile 
      mode."
  ; TODO: rename nodes?
  [n level]
  (cond
    (map-node? n)
    (let [level (condp = (node-type n)
                  :clojure/kernel/quote (inc level)
                  :clojure/kernel/unquote (if (has-attr? n :levels) 
                                            (- level (node-attr-value n :levels))
                                            (dec level))
                  level)
            ; _ (println "node:" (node-type n))
            ; _ (println "level:" level)
            ]
      (if (= level 0)
        ; handle "unquote" node now:
        (meta-compile (node-attr n :body))  ; what if the result is a raw value? 
                                            ; how does it get wrapped in nodes?
    
        ; otherwise recursively visit the children:
        `(make-node ~(node-type n)
                    ~(node-id n)
                    ~(mapfor [k (node-attrs n)] k (meta-compile-later (node-attr n k) level)))))
      ; (zipmap 
      ;   (keys n) 
      ;   (for [k (keys n)] 
      ;     (meta-compile-later (node-attr n k)))))
      
    (seq-node? n)
    `(make-node ~(node-type n)
                ~(node-id n)
                ~(vec (map #(meta-compile-later % level) (node-children n))))
    
    (node? n)
    `(make-node ~(node-type n)
                ~(node-id n)
                ~(node-value n))
    
    true
    n))

(defn unread
  "Wrap a Java/Clojure value in nodes (leaving nodes as is)."
  [r]
  (cond
    ; Note: this is a little wierd. Why not let the node just be the node?
    ; Yes, for now...
    (node? r)
    ; (make-node :clojure/kernel/quote {
    ;     :body
        r
      ; })
    
    (nil? r)
    (make-node :clojure/kernel/nil {})
    
    (= true r)
    (make-node :clojure/kernel/true {})
    
    (= false r)
    (make-node :clojure/kernel/false {})
    
    (integer? r)
    ; HACK: don't use unaryminus, because it means this resut is not in the kernel language...
    ; (if (< r 0)
    ;   (make-node :clojure/core/unaryminus {
    ;     :expr
    ;     (make-node :clojure/kernel/int { :value (- r) })
    ;   })
    ;   (make-node :clojure/kernel/int { :value r }))
    (make-node :clojure/kernel/int { :value r })
    
    (rational? r)
    (make-node :clojure/core/fraction {
      :num
      (unread (numerator r))
      
      :denom
      (unread (denominator r))
    })
    
    (string? r)
    (make-node :clojure/kernel/string { :value r })
      
    (keyword? r)
    (make-node :clojure/kernel/name { :value r })
      
    (seq? r)
    (make-node :clojure/core/list
      (vec (for [x r] (unread x))))
      
    true
    (node :view/chars 
      :str (str "???: " r) 
      :font :times)))

(defn meta-eval
  "Given a program in Clojure kernel syntax, compile it to raw Clojure forms,
  evaluate them, and return the resulting Java/Clojure value."
  [n]
  (let [ ; _ (print-node n true)
        c (meta-compile n)
        ; _ (println c)
        r (eval c) ]
    r))


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


; ; Hand-written reduction for the :clojure/kernel language; supplanted by the
; ; grammar in kernel.mlj
; (def kernelPresRules {
;   :clojure/kernel/bind
;   (fn [n] 
;     (node :view/expr/var :str (nameFromId (node-id n))))
;     
;   :clojure/kernel/var
;   (fn [n] 
;     (let [refNode (node-attr n :clojure/kernel/var/ref)  ; TODO: handle missing ref
;           refId (node-attr refNode :core/ref/id)]         ; TODO: handle missing id?
;       (node :view/expr/var :str (nameFromId refId))))
;       
;   ; ugly generic syntax with keywords "function" and ???:
;   :clojure/kernel/lambda
;   (fn [n]
;     (node :view/expr/flow
;       :boxes [
;         (node :view/expr/keyword :str "function")
;         (node :view/expr/juxt
;           :boxes
;           (vec (interpose 
;                   (node :view/sequence
;                     :items [
;                       (node :view/expr/keyword :str ",")
;                       (node :view/thinspace)
;                     ])
;                   (with-attr-seq n :clojure/kernel/lambda/params))))
;         (node :view/expr/symbol :str :to)
;         (with-attr-node n :clojure/kernel/lambda/body)
;       ]))
; 
;   ; Lambda-calculus-style syntax with lambda and ".":
;   ; :clojure/kernel/lambda
;   ; (fn [n]
;   ;   (node :view/expr/binary
;   ;     :boxes [
;   ;       ; (node :view/expr/keyword :str "lambda")
;   ;       (node :view/expr/symbol :str :lambda) ; HACK?
;   ;       (node :view/expr/juxt
;   ;         :boxes
;   ;         (vec (interpose 
;   ;                 (node :view/sequence
;   ;                   :items [
;   ;                     (node :view/expr/keyword :str ",")
;   ;                     (node :view/thinspace)
;   ;                   ])
;   ;                 (with-attr-seq n :clojure/kernel/lambda/params))))
;   ;       (node :view/expr/keyword :str ".")
;   ;       (with-attr-node n :clojure/kernel/lambda/body)
;   ;     ]))
;   ; 
;   :clojure/kernel/app  ; ugly generic syntax with the keywords "apply" and "to"
;   (fn [n]
;     (node :view/expr/flow
;       :boxes [
;         (node :view/expr/keyword :str "apply")
;         (with-attr-node n :clojure/kernel/app/expr)
;         (node :view/expr/keyword :str "to")
;         (node :view/expr/juxt
;           :boxes 
;           (vec (interpose 
;                   (node :view/sequence
;                     :items [
;                       (node :view/expr/keyword :str ",")
;                       (node :view/thinspace)
;                     ])
;                     (with-attr-seq n :clojure/kernel/app/args))))
;       ]))
;       
;   ; :clojure/kernel/app  ; "Haskell"-style application by juxtaposition (here, with medium space)
;   ; (fn [n]
;   ;   (node :view/expr/relation
;   ;     :boxes
;   ;     (vec (cons (with-attr-node n :clojure/kernel/app/expr) 
;   ;               (with-attr-seq n :clojure/kernel/app/args)))
;   ;     ))
;       
;   ; :clojure/kernel/app  ; "C"-style application with parens
;   ; (fn [n]
;   ;   (node :view/expr/juxt
;   ;     :boxes [
;   ;       (node-attr n :clojure/kernel/app/expr) 
;   ;       (node :view/parens
;   ;         :left "(" :right ")"
;   ;         :content 
;   ;           (first (node-attr n :clojure/kernel/app/args)))  ; HACK: one arg only!
;   ;     ]))
;       
;   :clojure/kernel/int
;   (fn [n]
;     (with-attr-node n :clojure/kernel/int/value v
;       (node :view/expr/int 
;         :str 
;         (str v))))
; 
;   :clojure/kernel/string
;   (fn [n]
;     (with-attr-node n :clojure/kernel/string/value v
;       (node :view/expr/string
;         :str 
;         (str v))))
;   
;   :clojure/kernel/true
;   (fn [n]
;     (node :view/expr/keyword 
;       :str "true"))
;   
;   :clojure/kernel/false
;   (fn [n]
;     (node :view/expr/keyword 
;       :str "false"))
;   
;   :clojure/kernel/nil
;   (fn [n]
;     (node :view/expr/keyword 
;       :str "nil"))
;   
;   :clojure/kernel/let
;   (fn [n]
;     (node :view/section
;       :items [
;         (node :view/expr/flow
;           :boxes [
;             (node :view/expr/keyword :str "let")
;             (with-attr-node n :clojure/kernel/let/bind)
;             (node :view/expr/symbol :str "=")
;             (with-attr-node n :clojure/kernel/let/expr)
;           ])
;         (node :view/sequence
;           :items [
;             (node :view/quad)
;             (node :view/expr/flow
;               :boxes [
;                 (node :view/expr/keyword :str "in")
;                 (with-attr-node n :clojure/kernel/let/body)
;               ])
;           ])
;       ]))
;       
;   :clojure/kernel/if
;   (fn [n]
;     (node :view/section
;       :items [
;         (node :view/expr/flow
;           :boxes [
;             (node :view/expr/keyword :str "if")
;             (with-attr-node n :clojure/kernel/if/test)
;           ])
;         (node :view/sequence
;           :items [
;           (node :view/quad)
;           (node :view/expr/flow
;             :boxes [
;               (node :view/expr/keyword :str "then")
;               (with-attr-node n :clojure/kernel/if/then)
;             ])
;           ])
;         (node :view/sequence
;           :items [
;           (node :view/quad)
;           (node :view/expr/flow
;             :boxes [
;               (node :view/expr/keyword :str "else")
;               (with-attr-node n :clojure/kernel/if/else)
;             ])
;           ])
;       ]))
;       
;   :clojure/kernel/extern
;   (fn [n]
;     (with-attr-node n :clojure/kernel/extern/name nm
;       (node :view/expr/mono
;         :str
;         (str "\"" nm "\""))))
; 
;   ; ; Backtick:
;   ; :core/later
;   ; (fn [n]
;   ;   (node :view/expr/juxt
;   ;     :boxes [
;   ;       (node :view/chars :str "\u00d2" :font :cmr10)
;   ;       (node-attr n :core/later/node)
;   ;     ]))
; 
;   ; Bordered:
;   ; :core/later
;   ; (fn [n]
;   ;   (node :view/border
;   ;     :weight 2
;   ;     :margin 0
;   ;     :view/drawable/colors [
;   ;       (node :view/gray :brightness 0.7)
;   ;     ]
;   ;       
;   ;     :item
;   ;     (node :view/border
;   ;       :weight 2
;   ;       :margin 2
;   ;       :view/drawable/colors [
;   ;         (node :view/gray :brightness 0.3)
;   ;       ]
;   ;       
;   ;       :item 
;   ;       (with-attr-node n :core/later/node))))
; 
;   ; Beveled:
;   :core/later
;   (fn [n]
;     (node :view/border
;       :weight 1
;       :margin 2
;       :view/drawable/colors [
;         (node :view/gray :brightness 0.5)
;         (node :view/gray :brightness 0.9)
;       ]
;       
;       :item 
;       (with-attr-node n :core/later/node)))
; 
;   ; Tilde:
;   ; :core/sooner
;   ; (fn [n]
;   ;   (node :view/expr/juxt
;   ;     :boxes [
;   ;       (node :view/chars :str "\u007e" :font :cmr10)
;   ;       (node-attr n :core/sooner/node)
;   ;     ]))
; 
;   ; Bordered:
;   ; :core/sooner
;   ; (fn [n]
;   ;   (node :view/border
;   ;     :weight 2
;   ;     :margin 0
;   ;     :view/drawable/colors [
;   ;       (node :view/gray :brightness 0.3)
;   ;     ]
;   ; 
;   ;     :item
;   ;     (node :view/border
;   ;       :weight 2
;   ;       :margin 2
;   ;       :view/drawable/colors [
;   ;         (node :view/gray :brightness 0.7)
;   ;       ]
;   ;       
;   ;       :item 
;   ;       (with-attr-node n :core/sooner/node))))
; 
;   ; Beveled:
;   :core/sooner
;   (fn [n]
;     (node :view/border
;       :weight 1
;       :margin 2
;       :view/drawable/colors [
;         (node :view/gray :brightness 0.9)
;         (node :view/gray :brightness 0.5)
;       ]
;       
;       :item 
;       (with-attr-node n :core/sooner/node)))
;       
; 
;   ; HACK: not really a kernel language construct
;   :clojure/kernel/program
;   (fn [n]
;     (node :view/section
;       :items 
;       (vec (interpose
;         (node :view/chars :str " " :font :cmr10)  ; HACK: a blank line, effectively
;         (with-attr-seq n :clojure/kernel/program/exprs)))))
; })


;
; Tests:
;

(deftest compile1
  (is (= 1
        (meta-compile 
          (make-node :clojure/kernel/int { :value 1 }))))
  (is (= true
        (meta-compile 
          (make-node :clojure/kernel/true))))
  (is (= "abc"
        (meta-compile 
          (make-node :clojure/kernel/string { :value "abc" })))))

; ; HACK
; (println (meta-compile
;           (make-node :clojure/kernel/app {
;               :expr
;               (make-node :clojure/kernel/lambda {
;                   :params
;                   (make-node :clojure/kernel/params [
;                       (make-node :clojure/kernel/bind :x {})
;                     ])
;               
;                   :body
;                   (make-node :clojure/kernel/var {
;                       :ref
;                       (ref-node :x)
;                     })
;                 })
;               
;               :args
;               (make-node :clojure/kernel/exprs [
;                   (make-node :clojure/kernel/int 42)
;                 ])
;             })))
; 
; ; HACK
; (println (meta-compile
;           (make-node :clojure/kernel/let {
;             :bind
;             (make-node :clojure/kernel/bind :x {})
;             
;             :expr
;             (make-node :clojure/kernel/int 1)
;             
;             :body
;             (make-node :clojure/kernel/var {
;               :ref
;               (ref-node :x)
;             })
;           })))
; 
; ; HACK
; (println 
;   (meta-compile
;           (make-node :clojure/kernel/let {
;             :bind
;             (make-node :clojure/kernel/bind :x {})
;             
;             :expr
;             (make-node :clojure/kernel/quote {
;               :body
;               (make-node :clojure/kernel/int 1)
;             })
;             
;             :body
;             (make-node :clojure/kernel/quote {
;               :body
;               (make-node :clojure/core/unaryminus {
;                 :expr
;                 (make-node :clojure/kernel/unquote {
;                   :body
;                   (make-node :clojure/kernel/var {
;                     :ref
;                     (ref-node :x)
;                   })
;                 })
;               })
;             })
;           })))

(deftest eval1
  (is (= 3
        (eval (meta-compile
          (make-node :clojure/kernel/app {
            :expr
            (make-node :clojure/kernel/extern { :name "+" })
            
            :args 
            (make-node :clojure/kernel/exprs [
              (make-node :clojure/kernel/int { :value 1 })
              (make-node :clojure/kernel/int { :value 2 })
            ])
          })))))
  (is (= 1
        (eval (meta-compile
          (node :clojure/kernel/let
            :bind
            (node :clojure/kernel/bind :core/id :x)
            
            :expr
            (make-node :clojure/kernel/int { :value 1 })
            
            :body
            (node :clojure/kernel/var
              :ref (ref-node :x)))))))
  (is (= 42
        (eval (meta-compile
          (make-node :clojure/kernel/app {
              :expr
              (make-node :clojure/kernel/lambda {
                  :params
                  (make-node :clojure/kernel/params [
                      (make-node :clojure/kernel/bind :x {})
                    ])
              
                  :body
                  (make-node :clojure/kernel/var {
                      :ref
                      (ref-node :x)
                    })
                })
              
              :args
              (make-node :clojure/kernel/exprs [
                  (make-node :clojure/kernel/int { :value 42 })
                ])
            })))))
  (is (= (make-node :foo)
        ((eval (meta-compile
                (make-node :clojure/kernel/lambda {
                  :params
                  (make-node :clojure/kernel/params [
                    (make-node :clojure/kernel/bind :x {})
                  ])
            
                  :body
                  (make-node :clojure/kernel/quote {
                    :body
                    (make-node :foo)
                  })
                }))) 1))))
                
(deftest quote1
  (is (= 1
        (eval (meta-compile
          (eval (meta-compile
            (make-node :clojure/kernel/quote {
              :body
              (make-node :clojure/kernel/int { :value 1 })
            })))))))
  (is (= 2
        (eval (meta-compile
          (eval (meta-compile
            (make-node :clojure/kernel/quote {
              :body
              (make-node :clojure/kernel/int {
                :value
                (make-node :clojure/kernel/unquote {
                  :body
                  (make-node :clojure/kernel/int { :value 2 })
                })
              })
            })))))))
  (is (= 7
        (eval (meta-compile
          (eval (meta-compile
            (make-node :clojure/kernel/quote {
              :body
              (make-node :clojure/kernel/int {
                :value
                (make-node :clojure/kernel/unquote {
                  :body
                  (make-node :clojure/kernel/app {
                    :expr
                    (make-node :clojure/kernel/extern { :name "+" })
          
                    :args 
                    (make-node :clojure/kernel/args [
                      (make-node :clojure/kernel/int { :value 3 })
                      (make-node :clojure/kernel/int { :value 4 })
                    ])
                  })
                })
              })
            }))))))))

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
                