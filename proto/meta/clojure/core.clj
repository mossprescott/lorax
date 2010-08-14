; Defines the constructs of Clojure, in terms of the kernel language.
; 
; This namespace is now just a couple of wrappers around the definitions given
; in the core.mlj grammar.

(ns meta.clojure.core
  (:use (meta core reduce check)
        (meta.clojure kernel)))

(def core-grammar
  (compose-grammars
    (load-node "meta/clojure/kernel2.mlj")
    (load-node "meta/clojure/core.mlj"))) ; HACK
    ; (load-node "meta/clojure/re.mlj")))

; (print-node core-grammar)
; (print-node (grammar-to-structure core-grammar))

(def core-checker
  ; (make-structure-checker (grammar-to-structure core-grammar)))
  (fn [n] {}))  ; HACK: no-op for now

(def core-display
  (grammar-to-display core-grammar))
