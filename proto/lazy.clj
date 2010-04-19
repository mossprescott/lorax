(def closure-sequence (for [ i (range 100) ] #(do (println "i:" i) i)))
(def ls (map #(apply % ()) closure-sequence))

(def ls2 (lazy-seq ((do (println "-1") 1) (do (println "-2") 2))))