; Program taking a seq of core/program files on the command line and simply 
; showing the contents.

(ns meta.util.showprogram
  (:use (meta core check reduce name)
        (meta.edit expr draw)
        (meta.clojure kernel core)))

(defn show-errors
  [errors]
  (doseq [ [id lst] errors ]
    (println id)
    (doseq [ e lst ] (println "  " e))))

(defn show
  [f]
  (let [pr (load-node f)
        errors (core-checker pr)
        _ (show-errors errors)
        display (compose-reductions name-to-expr #(meta-reduce2 % core-display))]
    (makeSyntaxFrame pr f display errors)))

(doseq [f *command-line-args*]
  (show f))