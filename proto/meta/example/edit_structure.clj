(ns meta.example.edit-structure
  (:use (clojure stacktrace)
        (meta.edit draw)
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
; (loadOne "meta/clojure/kernel2.mlj")


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
; (loadGrammar "meta/clojure/kernel2.mlj")

; Easier? Just a couple of simpler nodes:
(loadGrammar "meta/core.mlj")
(loadGrammar "meta/clojure/core.mlj")

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
