; Definitions for the core (language-independent) syntax.

; node-id and node-type get the mandatory attributes
; all other attributes can be retrieved with node-attr, or listed with node-attrs

; Node types:
; :core/ref - represents a pointer to another node
;   :core/ref/id -  

(ns meta.core
  (:use (clojure set test))
  (:require [clojure.zip :as zip])
  (:import (java.io 
            FileReader
            PushbackReader)))

;
; Some utilities for working with nodes:
;
(defn- genid
  [s]
  (keyword (str (gensym s))))

(defn node
  "Build a new node with the given type and children, and a freshly generated 
  id. If a :core/id child is provided, it overrides the generated id."
  [t & children]
  (let [ kwname #(subs (str %) 1) 
         childname (fn [t kw]
                      (if (some #{\/} (str kw))
                        kw
                        (keyword (str (kwname t) "/" (kwname kw))))) ]
    (apply hash-map 
      :core/type t,
      :core/id (genid "__"),
      (apply concat 
        (for [ [k v] (partition 2 children) ]
          [ (childname t k) v ])))))
      

(defn node? 
  "true if x represents an AST node."
  [x]
  (and (map? x) 
        (contains? x :core/type)))

(defn node-ref?
  "true if x is a reference-node (that is, a node that represents a pointer to another node)"
  [x]
  (and (map? x) 
        (= (x :core/type) :core/ref)))

(defn node-type 
  "The type of the node (a keyword)."
  [n]
  (do
    (if (not (node? n)) (println "not a node:" n))  ;; HACK: need a better assert
    (assert (node? n))
    (n :core/type)))

(defn node-id 
  "The id of the node (a keyword)."
  [n]
  (do
    (assert (node? n))
    (n :core/id)))
    
(defn node-attrs
	"List of attribute/field names."
	[n]
	(for [k (keys n) :when (not (#{ :core/type, :core/id } k))] k))

(defn node-attr
  [n attrName]
  (do
    (if (not (contains? n attrName))   ;; HACK: need a better assert
      (println "no attribute" attrName "in node" (node-type n) ";" (node-attrs n)))
    (assert (contains? n attrName))
    (n attrName)))

(defn node-children
	"List of child nodes (non-node-values ignored), including nodes in list-valued atrributes."
	[n]
  (apply concat
	  (for [a (node-attrs n)] 
	    (let [c (node-attr n a)]
  	    (cond
  	      (node? c) [c]
  	      (vector? c) (for [v c :when (node? v)] v)
  	      true [])))))

(defn visitNode
  "Applies a function to a node, recursively. The function is first applied to 
  the root node and the given value, yielding a new value. It is then applied to 
  each child node with _that_ value. The idea is that the function builds some
  kind of environment based on the recursive structure, and then uses that
  environment when it's invoked at each node. Note that this setup doesn't allow 
  the environment to be sensitive to the ordering of child nodes; every sibling
  is visited with the same environment.
  
  n: a node
  f: a function taking a node and arbitrary 'environment' value, and returning
    [ result, new environment value for child nodes ]
  env: 'environment' value
  
  Returns a sequence of the result values for each node, in no particular order,
  (but more or less in-order)."
  [n f env]
  (let [ [result newEnv] (f n env)]
    (apply concat 
      [result]
      (for [c (node-children n)]
        (visitNode c f newEnv)))))

(defn deep-node-ids
  "Seq of all ids found in the entire tree. Note: this could be trivially 
  constructed from visitNode now."
  [n]
  (visitNode n (fn [n env] [(node-id n) env]) nil))
  ; (reduce union #{(node-id n)}
  ;   (for [a (node-attrs n)]
  ;     (let [c (node-attr n a)]
  ;       (cond
  ;         (node? c) (deep-node-ids c)
  ;         (vector? c) (reduce union #{} (for [cn c] (deep-node-ids cn)))
  ;         true #{})))))


(defn find-node
  [n id]
  (first (for [n (visitNode n #(vec [%1 %2]) nil) :when (= (node-id n) id)] n)))

(defn check-refs
  "Validate the reference structure of the AST rooted at n. If id appears
  more than once or any ref refers to a non-existent id, an assertion failure 
  occurs."
  [n]
  (assert false))  ; TODO
  
  
(defn short-attr-name
  [n k]
  (let [nstr (str (node-type n))
        kstr (str k)]
    (if (.startsWith kstr nstr) 
      (str ":" (subs kstr (inc (count nstr)))) 
      kstr)))

(defn print-node
  "Prints a parsable (and vaguely human-readable) representation of the given
  node. Note: it's effectively a pretty-printed program using the (node) fn."
  ([n]
    (print-node n false))
  ([n allIds]
    (print-node n allIds ""))
  ([n allIds indent]
    (println (str indent "(node " (node-type n))) 
    (let [ indent (str "  " indent) ]
      (if (or allIds (not (.startsWith (str (node-id n)) ":_")))
        (println (str indent ":core/id " (node-id n))))
      (doseq [ k (keys n) :when (not (#{:core/type :core/id} k)) ]
        ; (println)
        (let [ kstr (short-attr-name n k) ]
          (do
            (cond
              (node? (n k))
              (do
                (println (str indent kstr))
                (print-node (n k) allIds indent))
          
              (vector? (n k))
              (do
                (println (str indent kstr " ["))
                (dorun
                  (map #(if (node? %) 
                          (print-node % allIds (str indent "  ")) 
                          (println (str indent "  " %))) 
                      (n k)))
                (println (str indent "]")))
          
              true
              (println (str indent kstr " " (n k))))))))
    (println (str indent ")"))))

(defn load-nodes
  "Read nodes from a '.mlj' file. Nodes can be in raw form (i.e. maps) or as 
  Clojure forms which evaluate to the actual nodes (i.e. '(node :foo ...)').
  Any form which isn't a node and isn't a (node ...) form is ignored."
  [fname]
  (let [r (PushbackReader. (FileReader. fname))
        nodes (loop [ v [] ]
                (let [ f (read r false :eof) ]
                  (if (= f :eof)
                    v
                    (cond 
                      (node? f) (recur (conj v f))
                      (and (list? f) (= (first f) 'node)) (recur (conj v (eval f)))
                      true (recur v)))))]
    (.close r)
    nodes))

;
; Grammar:
;

; Rules define:
; 1) what types represent valid nodes
; 2) what attributes must be present (and what their values must be?)
; 3) what child nodes must/may be present, and what their types should be

; Probably this eventually becomes the "core language" of rules,
; which will be enriched by a higher-level syntax for use by humans.


; TODO: not sure yet what form the productions should take, so this is just 
; meant to suggest the structure.

(def rules '{
  :core/ref 
  (rule :core/ref
    (:core/ref/id :core/keyword)),

  :core/later  ; "quote"
  (rule :core/later
    (:core/later/node (node :*))),
    
  :core/sooner  ; "unquote"
  (rule :core/sooner
    (:core/sooner/node (node :*))),
    
  ; that's it, I guess
})

;
; Tests:
;

(deftest children
  (let [n1 (node :foo)]
    (is (= [n1]
          (node-children (node :bar :baz n1))))))

(deftest deep-ids-1
  (is (= '(:1) 
      (deep-node-ids 
        (node :foo :core/id :1)))))
  
(deftest deep-ids-2
  (is (= #{:1 :2}
        (set (deep-node-ids 
              (node :foo :core/id :1 
                :child 
                (node :bar :core/id :2)))))))
  
(deftest deep-ids-3
  (is (= #{:1 :2 :3}
        (set (deep-node-ids 
              (node :foo :core/id :1 
                :child [
                  (node :bar :core/id :2)
                  (node :baz :core/id :3)
                ]))))))
