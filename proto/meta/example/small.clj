(ns meta.example.small
  (:use (meta core reduce)
        (meta.edit draw expr nodes)))
        ; (meta.clojure kernel)))


(def px
  (node :view/expr/juxt 
    :boxes [                  
      (node :view/expr/int :str "3")
      (node :view/expr/symbol :str :times)
      (node :view/expr/var :str "a")
    ]))
(print-node px true)
(let [ [pxr o] (meta-reduce2 px exprToView)] 
  (print-node pxr true)
  (println "o: " o))

