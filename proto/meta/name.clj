; Special support for reducing "names", which are the display form of ref-nodes 
; and the "bindings" they refer to.

(ns meta.name
	(:use (clojure test)
	      (meta core reduce)))

(defn- baseName
  "Remove any prefix from every name, to make things easy to read (but possibly 
  ambiguous). See check.clj"
  [kw]
  (let [#^String s (subs (str kw) 1)
        slashIdx (.lastIndexOf s (int \/))  ; is there a clean way to do this in Clojure?
        underIdx (.indexOf s (int \_))  ; maybe in contrib?
        startIdx (if (= slashIdx -1) 0 (inc slashIdx))
        endIdx (if (= underIdx -1) (.length s) underIdx)]
    ; (println s startIdx endIdx)
    (subs s startIdx endIdx)))
  ; (let [#^String s (subs (str kw) 1)
  ;       slashIdx (.lastIndexOf s (int \/))  ; is there a clean way to do this in Clojure?
  ;       underIdx (.lastIndexOf s (int \_))  ; maybe in contrib?
  ;       idx (max slashIdx underIdx)]
  ;   (if (= idx -1)
  ;       s
  ;       (subs s (inc idx)))))

; Map of node types to fxns which yield a node which represents the first 
; node's name:
(def findNameRules {
    :grammar/attr
    (fn [n] 
      (node-attr n :name))
      
    :clojure/kernel/bind
    (fn [n] n)
  })

; Map of node types to fxns which yield a (string) name for the given node,
; which is the one identified as the name for some node...
(def nameRules {
    :grammar/name
    (fn [n]
      (baseName (node-value n)))

    :clojure/kernel/bind
    (fn [n]
      (baseName (node-id n)))  ; HACK: look for an optional attr, or assign a random name (x, x', ...?)
  })


(defn- primed
  [name num]
  (let [n (make-node :view/expr/var { :str name })]
    (if (= num 0)
      n
      (make-node :view/scripted {
        :nucleus
        n
        
        :super
        (make-node :view/sequence 
          (repeat num (make-node :view/expr/prime)))
      }))))

(defn reduce-names
  "Reduce nodes which introduce 'bindings' and nodes that refer to them,
  given a fn producing a name (a simple string, for now)."
  ; TODO: uniquify names that are re-used (primes?)
  [root findRules rules]
  (let [vf (fn [n] 
              (if-let [fr (findRules (node-type n))]
                (let [nn (fr n)]
                  (if-let [nr (rules (node-type nn))]
                    [ (node-id n) (node-id nn) (nr nn) ]))))
        ts (filter #(not (nil? %)) (visitNode root vf))
        ; _ (println "ts:" ts)
        [ node-id-to-name-and-num name-id-to-name-and-num ]
                (loop [ts (seq ts) nodes {} names {} counts {}]
                  (if-not ts
                    [nodes names]
                    (let [ [nodeId nameNodeId name] (first ts) 
                           count (inc (get counts name -1))]
                      (recur (next ts) 
                              (assoc nodes nodeId [name count]) 
                              (assoc names nameNodeId [name count])
                              (assoc counts name count)))))
        ; id-to-name (reduce merge {} )  ; Note: (merge m nil) -> m
        ; _ (println "id-to-name-and-num:" id-to-name-and-num) ; HACK
        rf (fn [n]
              (if (ref-node? n)
                (if-let [ [name num] (node-id-to-name-and-num (ref-node-id n))]
                  (primed name num))
                (if-let [ [name num] (name-id-to-name-and-num (node-id n))]
                  (primed name num))))
        ]
    (meta-reduce2 root rf)))



(defn name-to-expr
  "Reduction for bindings to names. For the time being, the id is converted in 
  a fairly trivial way to a name, which will work only for hand-written nodes."
  [root]
  (reduce-names root findNameRules nameRules))
