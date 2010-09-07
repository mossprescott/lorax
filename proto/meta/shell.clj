; Functions for loading grammars and programs and displaying them, meant to be 
; used interactively (from the REPL).

; (ns meta.shell
;   (:use (meta core check reduce name)
;         (meta.edit expr draw)
;         (meta.clojure kernel)))

; Note: using raw 'use' insted of a proper namesapace because it's easier to 
; use in the REPL that way.
(use 'meta.core
     'meta.check
     'meta.reduce
     'meta.name
     'meta.edit.expr
     'meta.edit.draw)

; Atom containing a map of file names to atoms containing loaded programs.
; You can create a new world by redefining this var, but previously-created 
; views will still work.
(def cache
  (atom {}))


(defn load-program
  "Load an arbitrary program from a file, returning a (memoized) atom 
  containing the root node. This atom will be the point of synchronization for 
  the program -- edits replace the value, and observers use add-watch on it."
  [fname]
  (if-let [root (@cache fname)]
    root
    (let [root (atom (load-node fname))]
      (do 
        (println "loaded" fname)
        (swap! cache assoc fname root)
        ; (doseq [ [k v] @cache ] (println k "->" (node-type @v)))
        root))))


;
; Some common definitions:
;

(defn lift
  "Given an ordinary function and atoms containing arguments, returns a ref
  which contains the result of applying f to the arguments, which is updated 
  whenever any of the atoms is updated.
  Would it make more sense to return a fn that you apply to the lifted args?"
  [f & args]
  (let [r (ref nil)
        update (fn []
                  (dosync
                    (ref-set r 
                      (apply f (map deref args)))))]
    (update)
    (doseq [x args] (add-watch x r (fn [k r old new] (update))))
    r))

(defn load-grammars
  "Load one or more grammars from files, construct a single composite 
  grammar containing all the definitions, and return an atom containing it.
  The atom is updated when any grammar "
  [& fs]
  (apply lift compose-grammars (map load-program fs)))
  ; TODO: observe each atom and update self if any changes
  ; (let [a (atom nil)
  ;       pas (map load-program fs)]
  ;   (letfn [(lg [f]
  ;             (let [pa (load-program f)]
  ;               (add-watch pa a (fn [k r old new] build-grammar))
  ;               pa))
  ;            (build-grammar)
  ;             
  ;   (apply compose-grammars 
  ;     (map (fn [f] @(load-program f)) fs))))
    

; Not used at the moment (not checking syntax):
(def grammar-grammar 
  ; (load-grammar "meta/grammar.mlj" "meta/edit/view.mlj" "meta/edit/expr.mlj" 
  ;                   "meta/clojure/kernel.mlj" "meta/clojure/core.mlj"))
  (load-grammars "meta/clojure/kernel.mlj"))


; Temporary: using the hand-written reduction for grammars
(def grammar-display-simple
  (reduceByType (merge grammarPresRules structurePresRules)))

; ref containing the grammar for clojure, including kernel and core
(def clojure-grammar 
  ; (load-grammar "meta/core.mlj" "meta/clojure/kernel.mlj" "meta/clojure/core.mlj"))
  (load-grammars "meta/clojure/kernel.mlj" "meta/clojure/core.mlj"))
  
; ref containing the reduction function for clojure kernel/core
(def clojure-display
  (lift grammar-to-display clojure-grammar))
  ; (reduceByType kernelPresRules))


;
; Grammars:
;

; "display" fxn for grammars, built from:
;  - the "trickier "meta" reduction for :view/expr
;  - the name reduction
;  - one or more additional reductions
; This is a gigantic hack now. For one thing, the "meta-expr" reduction
; has to be provided with the set of source ids, which has to be captured 
; _before_ the name reduction runs...
(defn make-grammar-display
  [& rs]
  ; (println "rs:" rs)  ; HACK
  (let [dispFn (fn [n v]
                  (if-let [f (metaExprRules (node-type n))]
                    (f n v)
                    (let [; _ (print-node n true)
                          display (apply apply-until rs)
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

; "Standard" lifted grammar display function, including:
;  - the simple reductions for :structure and :grammar languages
;  - the display reductions for :clojure/kernel and /core
(def grammar-display
  (lift make-grammar-display 
        (ref grammar-display-simple) 
        clojure-display))

(defn show-grammar
  "Load a grammar from a file and display it in a regular editable syntax frame.
  Changes are reflected in any programs being displayed using the same grammar."
  [fname]
  (let [gr (load-program fname)
        ; _ (do (print "gr: ") (print-node gr true)) ; HACK
        errors {} ;(grammar-checker gr)  ; TODO
        ; _ (show-errors errors)
        ]
    (makeSyntaxFrame gr fname grammar-display errors)))

;
; Programs:
;



; (def core-display
;   (grammar-to-display clojure-grammar))

(defn show-program
  [fname]
  (let [pr (load-program fname)
        errors {} ; (core-checker pr)
        ; _ (show-errors errors)
        display (lift compose-reductions 
                      (ref name-to-expr) 
                      (lift (fn [r] #(meta-reduce2 % r)) clojure-display))]
    (makeSyntaxFrame pr fname display errors)))
