; Program taking a seq of core/program files on the command line and 
; evaluating each expr, displaying the result as a session.

(ns meta.util.showsession
  (:use (meta core check reduce name)
        (meta.edit expr draw)
        (meta.clojure kernel core run)))

; (def name-display
;   ; (let [f (reduceByType nameRules)]
;   ;   (fn [n] 
;   ;     (do 
;   ;       (println (node-type n)) 
;   ;       (f n)))))
;   (reduceByType nameRules))

(defn show-errors
  [errors]
  (doseq [ [id lst] errors ]
    (println id)
    (doseq [ e lst ] (println "  " e))))

(defn show
  [f]
  (let [pr (load-node f)
        sess (run-program pr expand-core)
        errors (core-checker sess)
        _ (show-errors errors)
        ; display #(meta-reduce2 % (apply-until name-display core-display))]
        display (compose-reductions name-to-expr #(meta-reduce2 % core-display))]
    (makeSyntaxFrame sess f display errors)))

(doseq [f *command-line-args*]
  (show f))