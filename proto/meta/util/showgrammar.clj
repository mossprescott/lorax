; Program taking one or more :grammar file names on the command line and 
; displaying an edit window for each.
;
; Currently uses the hand-written display reduction for the :grammar language;
; at some point it might be simpler to use the reduction in grammar.mlj.

(ns meta.util.showgrammar
  (:use (meta core check reduce name)
        (meta.edit expr draw)
        (meta.clojure kernel)))

(defn load-grammar
  [& fs]
  (apply compose-grammars 
    (map load-node fs)))

(def grammar-grammar 
  (load-grammar "meta/grammar.mlj" "meta/edit/view.mlj" "meta/edit/expr.mlj" 
                    "meta/clojure/kernel2.mlj" "meta/clojure/core.mlj"))

(def grammar-structure
  (grammar-to-structure grammar-grammar))

(def grammar-checker
  (make-structure-checker grammar-structure))

(def grammar-display-simple
  (reduceByType (merge grammarPresRules structurePresRules)))

(def clojure-grammar 
  (load-grammar "meta/core.mlj" "meta/clojure/kernel2.mlj" "meta/clojure/core.mlj"))
  
(def clojure-display
  (grammar-to-display clojure-grammar))
  ; (reduceByType kernelPresRules))

(def name-display
  (reduceByType nameRules))

; "display" fxn for grammars, built from:
;  - the simple reductions for :structure and :grammar languages
;  - the trickier reduction for :view/expr
;  - (eventually) the display reductions for :clojure/kernel and /core
(def grammar-display
  (let [dispFn (fn [n v]
                  (if-let [f (metaExprRules (node-type n))]
                    (f n v)
                    (let [; _ (print-node n true)
                          display (apply-until name-display 
                                              grammar-display-simple 
                                              clojure-display)
                          np (display n)
                          ; np (grammar-display-simple n)
                          ; _ (println "is gr/str:" (not (nil? np)))
                          ; npp (if (nil? np) 
                          ;         (clojure-display n) 
                          ;         np)
                          ; _ (if (nil? npp) (println "not reduced") (print-node npp true))
                          ; _ (println)
                          ]
                        [np v])))
        display (fn [n] 
                  (let [ v (set (deep-node-ids n))
                        [np o vp] (reduce-plus n dispFn v)]
                    [np o]))]
    display))

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
        errors (grammar-checker gr)
        _ (show-errors errors)]
    (makeSyntaxFrame gr f grammar-display errors)))

(doseq [f *command-line-args*]
  (show f))