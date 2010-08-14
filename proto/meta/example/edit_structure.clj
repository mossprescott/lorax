(ns meta.example.edit-structure
  (:use (clojure stacktrace)
        (meta.edit draw expr)
        (meta check core reduce)))

(def RULES structurePresRules)

(def checkStructureStructure (make-structure-checker (first (load-nodes "meta/structure.mlj"))))

(defn loadOne 
  [ fname ]
  (let [p (first (load-nodes fname))
        errors {}]; (checkStructureStructure p)] HACK
    ; (println "p" p)
    ; (print-node p)
    (doseq [ [k v] errors] (println k v))
    (makeSyntaxFrame 
      p fname (reduceByType RULES) errors)))
    
; (loadOne "meta/structure.mlj")
; (loadOne "meta/clojure/kernel.mlj")


(defn loadGrammar
  [fname]
  (let [grgr (first (load-nodes "meta/grammar.mlj"))
        display (grammar-to-display grgr)
        gr (first (load-nodes fname))
        red (grammar-to-structure gr)]
    (do
      ; (makeSyntaxFrame red (str fname " (reduced)") (reduceByType RULES) {})
      ; (print-node gr true)
      ; (println "ids:" (deep-node-ids gr))
      ; (println (meta-reduce2 gr (fn [n] nil)))
      (let [ [grp o] (meta-reduce2 gr display)]
        (println "result:")
        (print-node grp true))
        (makeSyntaxFrame gr fname display {})
      )))

; Grammar-language version of the kernel syntax, reduced to :structure lang.:
; (loadGrammar "meta/clojure/kernel.mlj")

; Easier? Just a couple of simpler nodes:
; (loadGrammar "meta/core.mlj")
; (loadGrammar "meta/clojure/core.mlj")

; (print-node (first (load-nodes "meta/grammar.mlj")))

; (let [red (grammar-to-structure (first (load-nodes "meta/grammar.mlj")))
;       _ (print-node red true)]
;   (makeSyntaxFrame red "grammar-reduced" (reduceByType RULES) (make-structure-checker struc)))

; (loadGrammar "meta/grammar.mlj")


; (def k (make-structure-checker (first (load-nodes "meta/kernel.mlj"))))
; 
; (println "int:" (k (node :clojure/kernel/int
;                     :core/id :1 
;                     :value 1)))
; (println "bad int:" (k (node :clojure/kernel/int 
;                     :core/id :2 
;                     :foo 1
;                     :bar 2
;                     :value [3])))

(defn show-errors
  [errors]
  (doseq [ [id lst] errors ]
    (println id)
    (doseq [ e lst ] (println "  " e))))

(defn showGrammar2
  [title gr]
  (let [;_ (do (print "source: ") (print-node gr true))
        mainGram (load-node "meta/grammar.mlj")
        viewGram (load-node "meta/edit/view.mlj")
        exprGram (load-node "meta/edit/expr.mlj")
        gram (compose-grammars mainGram viewGram exprGram)
        struc (grammar-to-structure gram)
        check (make-structure-checker struc)
        ; display (reduceOnce gr (reduceByType metaExprRules))
        
        ; this bit will eventually be replaced by the reduction defined in the grammar(s)
        grStDisplay (reduceByType (merge grammarPresRules structurePresRules))
        dispFn (fn [n v]
                  (if-let [f (metaExprRules (node-type n))]
                    (f n v)
                    [(grStDisplay n) v]))
        display (fn [n] 
                  (let [ v (set (deep-node-ids n))
                        [np o vp] (reduce-plus n dispFn v)]
                    [np o]))
        
        ; display (reduceByType structurePresRules)
        ; display (fn [n] nil)
        ; _ (println "displayfn:" display)
        ; _ (println "once:" (display gr))
        errors (check gr)
        _ (show-errors errors)
        ; _ (println "types:" (keys rules))
        ; _ (println "s-n:" ((rules :structure/node) (node :structure/node :type :foo)))
        ]
    (makeSyntaxFrame gr title display errors)))

(defn loadGrammar2
  [fname]
  (let [_ (println fname)
        gr (first (load-nodes fname))]
    (showGrammar2 fname gr)))
    
; (loadGrammar2 "meta/edit/view.mlj")  ; this one looks funny because the display reductions are bogus
; (loadGrammar2 "meta/edit/expr.mlj")  ; this one has slightly less bogus display reductions
; (loadGrammar2 "meta/clojure/kernel.mlj")  ; this one's for real
(loadGrammar2 "meta/clojure/core.mlj")  ; this one is real too

;(loadGrammar2 "meta/grammar.mlj")

; (showGrammar2 
;   "dumb"
;   (node :grammar/language
;     :rules [
;       (node :grammar/rule
;         :type
;         :foo/one
;         
;         :supers []
;         
;         :display
;         (node :view/expr/flow
;           :boxes [
;             (node :view/expr/keyword :str "one")
;             (node :view/expr/int :str "1")
;           ]))
;           
;       (node :grammar/rule
;         :type
;         :foo/list
;         
;         :supers [ :foo/thing ]
;         
;         :display
;         (node :view/parens
;           :left "["
;           :right "]"
;           
;           :content 
;           (node :view/expr/relation
;             :boxes
;             (node :grammar/sequence
;               :name
;               :items
;               
;               :options [
;                 (node :structure/node :type :foo/listitem)
;               ]
;               
;               :min 0
;               
;               :separator
;               (node :view/expr/keyword :str ", ")
;               ))))
;     ]))