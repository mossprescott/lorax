; This file will contain definitions pertaining to the kernel language, and 
; perhaps the translation from the kernel language to ordinary Clojure forms.

(ns meta.clojure.kernel
  (:use (clojure test)
        (meta core reduce)))
  


;
; Compilation from nodes of :kernel language to ordinary Clojure forms:
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
      :kernel/bind
      (symbolFromId (node-id n))
        
      :kernel/lambda
      `(fn ~(symbolFromId (node-id n))
          [ ~@(map meta-compile (node-children (node-attr n :params))) ]
          ~(meta-compile (node-attr n :body)))
        
      :kernel/recur
      `(recur
        ~@(map meta-compile (node-children (node-attr n :args))))
      
      :kernel/app
      `(~(meta-compile (node-attr n :expr)) 
        ~@(map meta-compile (node-children (node-attr n :args))))
          
      :kernel/if
      `(if ~(meta-compile (node-attr n :test))
        ~(meta-compile (node-attr n :then))
        ~(meta-compile (node-attr n :else)))

      :kernel/let
      ; (let [q  ; HACK
      `(let [ ~(symbolFromId (-> n (node-attr :bind) node-id))
                ~(meta-compile (node-attr n :expr)) ]
        ~(meta-compile (node-attr n :body)))
      ; ] (do (println "let:" q) q))  ; HACK
         
      :kernel/var
      ; (do (println "var:" n)  ; HACK
      (symbolFromId (-> n (node-attr :ref) ref-node-id))
      ; ) ; HACK
        
      :kernel/true true
      :kernel/false false
      :kernel/nil nil
        
      :kernel/int
      (node-attr-value n :value)
        
      :kernel/string
      (node-attr-value n :value)
        
      :kernel/extern
      ; (do (println "extern:" n)  ; HACK
      (symbol (node-attr-value n :name))
      ; ) ; HACK
      
      :kernel/name
      (node-attr-value n :value)
      
      ; t :lorax/later
      ;        ; Note: nodes are represented as maps, and therefore implicitly quoted
      ;        ; at the Clojure level.
      ;        ; TODO: need syntax quoting here, so embedded unquotes will be evaluated
      ;        (meta-compile-later (node-attr n :lorax/later/node))

      :kernel/quote
      (meta-compile-later (node-attr n :body) 1)
      
;      :kernel/unquote ; should never be encountered here?
      
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
                  :kernel/quote (inc level)
                  :kernel/unquote (if (has-attr? n :levels) 
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
    ; (make-node :kernel/quote {
    ;     :body
        r
      ; })
    
    (nil? r)
    (make-node :kernel/nil {})
    
    (= true r)
    (make-node :kernel/true {})
    
    (= false r)
    (make-node :kernel/false {})
    
    (integer? r)
    ; HACK: don't use unaryminus, because it means this resut is not in the kernel language...
    ; (if (< r 0)
    ;   (make-node :core/unaryminus {
    ;     :expr
    ;     (make-node :kernel/int { :value (- r) })
    ;   })
    ;   (make-node :kernel/int { :value r }))
    (make-node :kernel/int { :value r })
    
    (rational? r)
    (make-node :core/fraction {
      :num
      (unread (numerator r))
      
      :denom
      (unread (denominator r))
    })
    
    (string? r)
    (make-node :kernel/string { :value r })
      
    (keyword? r)
    (make-node :kernel/name { :value r })
      
    (seq? r)
    (make-node :core/list
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


;
; Tests:
;

(deftest compile1
  (is (= 1
        (meta-compile 
          (make-node :kernel/int { :value 1 }))))
  (is (= true
        (meta-compile 
          (make-node :kernel/true))))
  (is (= "abc"
        (meta-compile 
          (make-node :kernel/string { :value "abc" })))))

; ; HACK
; (println (meta-compile
;           (make-node :kernel/app {
;               :expr
;               (make-node :kernel/lambda {
;                   :params
;                   (make-node :kernel/params [
;                       (make-node :kernel/bind :x {})
;                     ])
;               
;                   :body
;                   (make-node :kernel/var {
;                       :ref
;                       (ref-node :x)
;                     })
;                 })
;               
;               :args
;               (make-node :kernel/exprs [
;                   (make-node :kernel/int 42)
;                 ])
;             })))
; 
; ; HACK
; (println (meta-compile
;           (make-node :kernel/let {
;             :bind
;             (make-node :kernel/bind :x {})
;             
;             :expr
;             (make-node :kernel/int 1)
;             
;             :body
;             (make-node :kernel/var {
;               :ref
;               (ref-node :x)
;             })
;           })))
; 
; ; HACK
; (println 
;   (meta-compile
;           (make-node :kernel/let {
;             :bind
;             (make-node :kernel/bind :x {})
;             
;             :expr
;             (make-node :kernel/quote {
;               :body
;               (make-node :kernel/int 1)
;             })
;             
;             :body
;             (make-node :kernel/quote {
;               :body
;               (make-node :core/unaryminus {
;                 :expr
;                 (make-node :kernel/unquote {
;                   :body
;                   (make-node :kernel/var {
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
          (make-node :kernel/app {
            :expr
            (make-node :kernel/extern { :name "+" })
            
            :args 
            (make-node :kernel/exprs [
              (make-node :kernel/int { :value 1 })
              (make-node :kernel/int { :value 2 })
            ])
          })))))
  (is (= 1
        (eval (meta-compile
          (node :kernel/let
            :bind
            (node :kernel/bind :lorax/id :x)
            
            :expr
            (make-node :kernel/int { :value 1 })
            
            :body
            (node :kernel/var
              :ref (ref-node :x)))))))
  (is (= 42
        (eval (meta-compile
          (make-node :kernel/app {
              :expr
              (make-node :kernel/lambda {
                  :params
                  (make-node :kernel/params [
                      (make-node :kernel/bind :x {})
                    ])
              
                  :body
                  (make-node :kernel/var {
                      :ref
                      (ref-node :x)
                    })
                })
              
              :args
              (make-node :kernel/exprs [
                  (make-node :kernel/int { :value 42 })
                ])
            })))))
  (is (= (make-node :foo)
        ((eval (meta-compile
                (make-node :kernel/lambda {
                  :params
                  (make-node :kernel/params [
                    (make-node :kernel/bind :x {})
                  ])
            
                  :body
                  (make-node :kernel/quote {
                    :body
                    (make-node :foo)
                  })
                }))) 1))))
                
(deftest quote1
  (is (= 1
        (eval (meta-compile
          (eval (meta-compile
            (make-node :kernel/quote {
              :body
              (make-node :kernel/int { :value 1 })
            })))))))
  (is (= 2
        (eval (meta-compile
          (eval (meta-compile
            (make-node :kernel/quote {
              :body
              (make-node :kernel/int {
                :value
                (make-node :kernel/unquote {
                  :body
                  (make-node :kernel/int { :value 2 })
                })
              })
            })))))))
  (is (= 7
        (eval (meta-compile
          (eval (meta-compile
            (make-node :kernel/quote {
              :body
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
            }))))))))

; (deftest quote1
;   (let [n (node :kernel/let
;             :bind
;             (node :kernel/bind :lorax/id :x)
;             
;             :expr
;             (node :kernel/int :value 1)
;             
;             :body
;             (node :kernel/quote
;               :body
;               (node :kernel/app
;                 :expr
;                 (node :kernel/extern
;                   :name "+")
;                   
;                 :args [
;                   (node :kernel/unquote
;                     :body
;                     (node :kernel/var
;                       :ref (ref-node :x)))
;                   (node :kernel/int
;                     :value 2)
;                 ])))
;           _ (println (meta-compile n))
;           _ (print-node (meta-eval n) true)]
;     (is (= 1
;           (meta-compile (meta-eval n))))))
                