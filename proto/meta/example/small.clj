(ns meta.example.small
  (:use (meta.edit draw expr nodes)
        (meta core reduce kernel)))


(def px
  (node :view/expr/juxt 
    :boxes [                  
      (node :view/expr/int :str "3")
      ; (node :view/expr/symbol :str :times)
      (node :view/expr/var :str "a")
    ]))
(print-node px true)
(let [ [pxr o] (meta-reduce2 px (reduceByType exprRules))] 
  (print-node pxr true)
  (println "o: " o))

