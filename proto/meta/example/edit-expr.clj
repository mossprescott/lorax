(ns meta.example.edit-expr
  (:use (meta.edit draw expr nodes)
        (meta core reduce kernel)))

; (println "seqsize" (size (node :view/sequence :items []) nil nil))

(def fact
  (node :view/expr/flow
    :boxes [
      (node :view/expr/juxt
        :boxes [
          (node :view/expr/var :str "n")
          (node :view/expr/symbol :str "!")
        ])

      (node :view/expr/symbol :str :to)  ; some other symbol for "definition"?
      
    (node :view/section
      :items [
        (node :view/expr/flow
          :boxes [
            (node :view/expr/keyword :str "if")
            (node :view/expr/relation
              :boxes [
                (node :view/expr/var :str "n")
                (node :view/expr/symbol :str "=")
                (node :view/expr/int :str "0")
              ])
          ])
        (node :view/sequence
          :items [
            (node :view/quad)
            (node :view/expr/flow
              :boxes [
                (node :view/expr/keyword :str "then")
                (node :view/expr/int :str "1")
              ])
          ])
        (node :view/sequence
          :items [
            (node :view/quad)
            (node :view/expr/flow
              :boxes [
                (node :view/expr/keyword :str "else")
                (node :view/expr/binary
                  :boxes [
                    (node :view/expr/var :str "n")
                    (node :view/expr/symbol :str :times)
                    (node :view/expr/juxt
                      :boxes [
                        (node :view/expr/binary
                          :boxes [
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
  (node :view/section
    :items [
    (node :view/expr/flow
      :boxes[
      (node :view/expr/keyword :str "let")
      (node :view/expr/var :str "x")
      (node :view/expr/symbol :str "=")
      (node :view/expr/binary
        :boxes [
          (node :view/expr/int :str "1")
          (node :view/expr/symbol :str "+")
          (node :view/expr/int :str "2")
        ])
    ])
    
    (node :view/sequence
      :items [
        (node :view/quad)
        (node :view/section
          :items [
            (node :view/expr/flow
              :boxes [
              (node :view/expr/keyword :str "in")
              (node :view/expr/binary
                :boxes [
                  (node :view/expr/int :str "3")
                  (node :view/expr/symbol :str :times)
                  (node :view/expr/binary 
                    :boxes [                  
                      (node :view/expr/var :str "x")
                      (node :view/expr/symbol :str "+")
                      (node :view/expr/juxt 
                        :boxes [                  
                          (node :view/expr/int :str "3")
                          ; (node :view/expr/symbol :str :times)
                          (node :view/expr/var :str "a")
                        ])
                      ])
                ])
              ])
            ])
          ])
          
      fact
      ]))

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

(makeFrame p2 "view/expr/...")

