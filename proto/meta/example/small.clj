(ns meta.example.small
  (:use (meta core reduce)
        (meta.edit draw expr nodes)))
        ; (meta.clojure kernel)))


(def px
  (make-node :view/expr/juxt [
      (make-node :view/expr/int { :str "3" })
      (make-node :view/expr/symbol { :str :times })
      (make-node :view/expr/var { :str "a" })
    ]))
(print-node px true)
(let [ [pxr o] (exprToView px)] 
  (print-node pxr true)
  (println "o: " o))

