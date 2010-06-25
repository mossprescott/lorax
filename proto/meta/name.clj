; Special support for reducing "names", which are the display form of ref-nodes 
; and the "bindings" they refer to.

(ns meta.name
	(:use (clojure test)
	      (meta core reduce)))

(defn- simple-name
  [kw]
  (subs (str kw) 1))

(def nameRules {
    :clojure/kernel/bind
    (fn [n]
      ; (do (println "bind" (node-id n))
      (node :view/expr/var
        :str (simple-name (node-id n))))
      ; )
      
    :clojure/kernel/var
    (fn [n]
      (let [r (node-attr n :ref)]
        (node :view/expr/var
          :str (simple-name (ref-node-id r)))))
  })
        
(defn name-to-expr
  "Reduction for bindings to names. For the time being, the id is converted in 
  a fairly trivial way to a name, which will work only for hand-written nodes."
  [n]
  (meta-reduce2 n (reduceByType nameRules)))