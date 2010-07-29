(ns meta.example.edit-expr
  (:use (meta.edit draw expr nodes)
        (meta core reduce)
        (meta.clojure kernel)))

; (println "seqsize" (size (node :view/sequence :items []) nil nil))

(def fact
  (make-node :view/expr/flow [
      (make-node :view/expr/juxt [
          (node :view/expr/var :str "n")
          (node :view/expr/symbol :str "!")
        ])

      (node :view/expr/symbol :str :to)  ; some other symbol for "definition"?
      
    (make-node :view/section [
        (make-node :view/expr/flow [
            (make-node :view/expr/keyword { :str "if" })
            (make-node :view/expr/relation [
                (node :view/expr/var :str "n")
                (node :view/expr/symbol :str "=")
                (node :view/expr/int :str "0")
              ])
          ])
        (make-node :view/sequence [
            (node :view/quad)
            (make-node :view/expr/flow [
                (node :view/expr/keyword :str "then")
                (node :view/expr/int :str "1")
              ])
          ])
        (make-node :view/sequence [
            (node :view/quad)
            (make-node :view/expr/flow [
                (node :view/expr/keyword :str "else")
                (make-node :view/expr/binary [
                    (node :view/expr/var :str "n")
                    (node :view/expr/symbol :str :times)
                    (make-node :view/expr/juxt [
                        (make-node :view/expr/binary [
                            (node :view/expr/var :str "n")
                            (node :view/expr/symbol :str "-")
                            (node :view/expr/int :str "1")
                          ])
                        (node :view/expr/symbol :str "!")
                      ])
                  ])
              ])
          ])
      ])
  ]))
        
(def p2
  (make-node :view/section [
    (make-node :view/expr/flow [
        (node :view/expr/keyword :str "let")
        (node :view/expr/var :str "x")
        (node :view/expr/symbol :str "=")
        (make-node :view/expr/binary [
            (node :view/expr/int :str "1")
            (node :view/expr/symbol :str "+")
            (node :view/expr/int :str "2")
          ])
      ])
    
    (make-node :view/sequence [
        (node :view/quad)
        (make-node :view/section [
            (make-node :view/expr/flow [
                (node :view/expr/keyword :str "in")
                (make-node :view/expr/binary [
                    (node :view/expr/int :str "3")
                    (node :view/expr/symbol :str :times)
                    (make-node :view/expr/binary [
                        (node :view/expr/var :str "x")
                        (node :view/expr/symbol :str "+")
                        (make-node :view/expr/juxt [
                            (node :view/expr/int :str "3")
                            ; (node :view/expr/symbol :str :times)
                            (node :view/expr/var :str "a")
                          ])
                      ])
                  ])
              ])
          ])
      ])

      (node :view/quad)  ; need vspace?

      fact
  ]))

; (def p2 
;   (make-node :view/expr/flow [
;       (make-node :view/expr/keyword { :str "let" })
;       (make-node :view/expr/var { :str "x" })
;       (make-node :view/expr/symbol { :str "=" })
;       (make-node :view/expr/binary [
;           (make-node :view/expr/int { :str "1" })
;           (make-node :view/expr/symbol { :str "+" })
;           (make-node :view/expr/int { :str "2" })
;         ])
;     ]))

; (println "1")
; (def p2-reduced (meta-reduce p2 exprRules))
; ; (print-node p2-reduced)
; 
; (println "2")
; (def p2p (parenthesize p2))
; (println "3")
; (print-node p2p)
; 
; (println "4")
; (def p2p-reduced (meta-reduce p2p exprRules))
; (println "5")
; (print-node p2p-reduced)

; (makeFrame p2 "view/expr/...")
(makeSyntaxFrame p2 "view/expr/..." (fn [n] (meta-reduce2 n (reduceByType {}))) {})
