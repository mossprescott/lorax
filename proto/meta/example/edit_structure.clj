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
    
(loadOne "meta/structure.mlj")
(loadOne "meta/clojure/kernel.mlj")
; (loadOne "meta/clojure/kernel2.mlj")


(let [red (grammar-to-structure (first (load-nodes "meta/clojure/kernel2.mlj")))]
  (makeSyntaxFrame red "kernel2-reduced" (reduceByType RULES) {}))

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
