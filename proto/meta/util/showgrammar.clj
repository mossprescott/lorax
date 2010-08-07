; Program taking one or more :grammar file names on the command line and 
; displaying an edit window for each.
;
; Currently uses the hand-written display reduction for the :grammar language;
; at some point it might be simpler to use the reduction in grammar.mlj.

(set! *warn-on-reflection* true)

(ns meta.util.showgrammar
  (:use (meta core check reduce name)
        (meta.edit expr draw)
        (meta.clojure kernel)))

(defn load-grammar
  [& fs]
  (apply compose-grammars 
  ; (dorun (map print-node
  ; (println
    (map load-node fs)))
    

(def grammar-grammar 
  ; (load-grammar "meta/grammar.mlj" "meta/edit/view.mlj" "meta/edit/expr.mlj" 
  ;                   "meta/clojure/kernel2.mlj" "meta/clojure/core.mlj"))
  (load-grammar "meta/clojure/kernel2.mlj"))

; (def grammar-structure
;   (grammar-to-structure grammar-grammar))

; (def grammar-checker
;   (make-structure-checker grammar-structure))

(def grammar-display-simple
  (reduceByType (merge grammarPresRules structurePresRules)))

(def clojure-grammar 
  ; (load-grammar "meta/core.mlj" "meta/clojure/kernel2.mlj" "meta/clojure/core.mlj"))
  (load-grammar "meta/core.mlj" "meta/clojure/kernel2.mlj"))
  
(def clojure-display
  (grammar-to-display clojure-grammar))
  ; (reduceByType kernelPresRules))

; (def name-display
;   (reduceByType nameRules))

; "display" fxn for grammars, built from:
;  - the trickier reduction for :view/expr
;  - the name reduction
;  - the simple reductions for :structure and :grammar languages
;  - (eventually) the display reductions for :clojure/kernel and /core
; This is a gigantic hack now. For one thing, the "meta-expr" reduction
; has to provided with the set of source ids, which has to be captured _before_
; the name reduction runs...
(def grammar-display
  (let [dispFn (fn [n v]
                  (if-let [f (metaExprRules (node-type n))]
                    (f n v)
                    (let [; _ (print-node n true)
                          display grammar-display-simple 
                                    ; (apply-until grammar-display-simple 
                                              ; clojure-display)
                          np (display n)
                          ]
                        [np v])))
        d (fn [n]
            (let [ v (set (deep-node-ids n))
                   display (fn [n] 
                               (let [ [np o vp] (reduce-plus n dispFn v) ]
                                 [np o]))
                   d2 (compose-reductions name-to-expr display)]
              (d2 n)))]
      d))

; (defn showGrammar2
;   [title gr]
;   (let [;_ (do (print "source: ") (print-node gr true))
;         mainGram (load-node "meta/grammar.mlj")
;         viewGram (load-node "meta/edit/view.mlj")
;         exprGram (load-node "meta/edit/expr.mlj")
;         gram (compose-grammars mainGram viewGram exprGram)
;         struc (grammar-to-structure gram)
;         check (make-structure-checker struc)
;         ; display (reduceOnce gr (reduceByType metaExprRules))
;         
;         ; this bit will eventually be replaced by the reduction defined in the grammar(s)
;         grStDisplay (reduceByType (merge grammarPresRules structurePresRules))
;         dispFn (fn [n v]
;                   (if-let [f (metaExprRules (node-type n))]
;                     (f n v)
;                     [(grStDisplay n) v]))
;         display (fn [n] 
;                   (let [ v (set (deep-node-ids n))
;                         [np o vp] (reduce-plus n dispFn v)]
;                     [np o]))
;         
;         ; display (reduceByType structurePresRules)
;         ; display (fn [n] nil)
;         ; _ (println "displayfn:" display)
;         ; _ (println "once:" (display gr))
;         errors (check gr)
;         _ (show-errors errors)
;         ; _ (println "types:" (keys rules))
;         ; _ (println "s-n:" ((rules :structure/node) (node :structure/node :type :foo)))
;         ]
;     (makeSyntaxFrame gr title display errors)))
; 
; (defn loadGrammar2
;   [fname]
;   (let [_ (println fname)
;         gr (first (load-nodes fname))]
;     (showGrammar2 fname gr)))

(defn show-errors
  [errors]
  (doseq [ [id lst] errors ]
    (println id)
    (doseq [ e lst ] (println "  " e))))

(defn show
  [f]
  (let [gr (load-node f)
        ; _ (do (print "gr: ") (print-node gr true)) ; HACK
        errors {} ;(grammar-checker gr)
        _ (show-errors errors)]
    (makeSyntaxFrame gr f grammar-display errors)))

(doseq [f *command-line-args*]
  (show f))