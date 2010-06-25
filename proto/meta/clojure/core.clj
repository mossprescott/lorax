; Defines the constructs of Clojure, in terms of the kernel language.
; Because these constructs will probably be useful for writing most
; reductions, they may need to be defined this way, in raw Clojure, 
; or else in meta-Clojure but using only the kernel forms, or else 
; as a sequence of definitions, each depending on the earlier ones
; (that's essentially what happens in real LISPs).
; That last alternative is attractive, but maybe too complex?

(ns meta.clojure.core
  (:use (meta core reduce check)
        (meta.clojure kernel)))

(def core-grammar
  (compose-grammars
    (load-node "meta/clojure/kernel2.mlj")
    (load-node "meta/clojure/core.mlj")))

; (print-node core-grammar)
; (print-node (grammar-to-structure core-grammar))

(def core-checker
  (make-structure-checker (grammar-to-structure core-grammar)))

(def core-display
  (grammar-to-display core-grammar))


; 
; ; Rules for use with meta-reduce, given as ordinary Clojure fns
; (def ccrules {
;   ; logical "and" with 2 args, "left" and "right"
;   ; this version is wrong: it doesn't generate a new id for the bound var
;   :clojure/core/and2-wrong
;   (fn [n]
;     (node :clojure/kernel/let
;       :bind 
;       (node :core/bind :core/id :and)
;       
;       :expr
;       (n :clojure/core/and2-wrong/left)
;     
;       :body
;       (node :clojure/kernel/if
;         :test 
;         (node :clojure/kernel/var
;           :ref
;           (node :core/ref
;             :id :and))
; 
;         :then
;         (n :clojure/core/and2-wrong/right)
;       
;         :else
;         (node :clojure/kernel/var
;           :ref
;           (node :core/ref
;             :id :and)))))
;   
;   ; logical "and" with 2 args, "left" and "right"
;   :clojure/core/and2
;   (fn [n]
;     (let [ newLeft (genid "and") ]  ; generate a new, unique id
;       (node :clojure/kernel/let
;         :bind 
;         (node :core/bind :core/id newLeft)  ; use the new id for this binding
;         
;         :expr
;         (n :clojure/core/and2/left)
;       
;         :body
;         (node :clojure/kernel/if
;           :test 
;           (node :clojure/kernel/var
;             :ref
;             (node :core/ref
;               :id newLeft))
; 
;           :then
;           (n :clojure/core/and2/right)
;         
;           :else
;           (node :clojure/kernel/var
;             :ref
;             (node :core/ref
;               :id newLeft))))))
;               
;   ; :clojure/core/and2-meta
;   ; (eval (meta-compile
;   ;   (node :clojure/kernel/lambda
;   ;     :params [
;   ;     (node :clojure/bind :core/id :n)
;   ;     ]
;   ;     
;   ;     :body
;   ;     (node :core/later
;   ;       :expr
;   ;       (node :clojure/kernel/let
;   ;         :bind 
;   ;         (node :core/bind :core/id :left)
;   ;         
;   ;         :expr
;   ;         (node :core/sooner
;   ;           :expr
;   ;           (node :clojure/kernel/var
;   ;             :ref
;   ;             (node :core/ref :id :n)))
;   ;         
;   ;         :body
;   ;         (node :clojure/kernel/if
;   ;           :test
;   ;           (node :clojure/kernel/var
;   ;             :ref
;   ;             (node :core/ref :id :left))
;   ;           
;   ;           :then
;   ;           (node :core/sooner
;   ;             :expr
;   ;             (node :clojure/kernel/var
;   ;               :ref
; })
; 
