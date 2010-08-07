; Special support for reducing "names", which are the display form of ref-nodes 
; and the "bindings" they refer to.

(ns meta.name
	(:use (clojure test)
	      (meta core reduce)))

(defn- simple-name
  [kw]
  (subs (str kw) 1))

(defn- baseName
  "Remove any prefix from every name, to make things easy to read (but possibly 
  ambiguous). See check.clj"
  [kw]
  (let [#^String s (subs (str kw) 1)
        idx (.lastIndexOf s (int \/))]  ; a clean way to do this in Clojure?
    (if (= idx -1)
        s
        (subs s (inc idx)))))

; (def nameRules {
;     :clojure/kernel/bind
;     (fn [n]
;       ; (do (println "bind" (node-id n))
;       (make-node :view/expr/var {
;           :str (simple-name (node-id n))
;         }))
;       ; )
;       
;     :clojure/kernel/var
;     (fn [n]
;       (let [r (node-attr n :ref)]
;         (make-node :view/expr/var {
;             :str (simple-name (ref-node-id r))
;           })))
;   })

; Map of node types to fxns which yield a node which represents the first 
; node's name:
(def findNameRules {
    :grammar/attr
    (fn [n] 
      (node-attr n :name))
  })

; Map of node types to fxns which yield a (string) name for the given node,
; which is the one identified as the name for some node...
(def nameRules {
    :grammar/name
    (fn [n]
      (baseName (node-value n)))
  })


(defn reduce-names
  "Reduce nodes which introduce 'bindings' and nodes that refer to them,
  given a fn producing a name (a simple string, for now)."
  ; TODO: uniquify names that are re-used (primes?)
  [root findRules rules]
  (let [vf (fn [n env] 
              (if-let [fr (findRules (node-type n))]
                (let [nn (fr n)]
                  (if-let [nr (rules (node-type nn))]
                    [{(node-id n) (nr nn)} nil]
                    [{} nil]))
                [{} nil]))
        id-to-name (reduce merge {} (visitNode root vf nil))
        ; _ (println "id-to-name:" id-to-name)
        rf (fn [n]
              (if (ref-node? n)
                (if-let [name (id-to-name (ref-node-id n))]
                  (make-node :view/expr/var { :str name }))
                (if-let [nf (rules (node-type n))]
                  (make-node :view/expr/var { :str (nf n) }))))
        ]
    (meta-reduce2 root rf)))



(defn name-to-expr
  "Reduction for bindings to names. For the time being, the id is converted in 
  a fairly trivial way to a name, which will work only for hand-written nodes."
  [root]
  (reduce-names root findNameRules nameRules))
