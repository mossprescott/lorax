; Definitions for the constructing and using nodes using a simple concrete
; embedding. These functions encapaulate the encoding of nodes; all other 
; code working with nodes should be based on them to get a measure of 
; abstraction/implementation independence.

(ns meta.core
  (:use (clojure set test))
  (:require [clojure.zip :as zip])
  (:import (java.io 
            FileReader
            PushbackReader)))

;
; Some utilities for working with nodes:
;
(defn genid
  "A new, globally unique node id."
  ([]
    (genid "__"))
  ([s]
    (keyword (str (gensym s)))))

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
      :core/id (genid),
      (apply concat 
        (for [ [k v] (partition 2 children) ]
          [ (childname t k) v ])))))
      

(defn node? 
  "True if x represents an AST node (including ref nodes)."
  [x]
  (and (map? x) 
        (contains? x :core/type)))

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
    
(defn ref-node?
  "true if x is a reference-node (that is, a node that represents a pointer to another node)"
  [x]
  (and (map? x) 
        (= (x :core/type) :core/ref)))

(defn ref-node-id
  "Id of the node pointed to by the given reference-node."
  [n]
  (do
    (assert (ref-node? n))
    (n :core/ref/id)))

(defn node-attrs
	"List of attribute/field names."
	[n]
	(do
    (assert (node? n))
	  (for [k (keys n) :when (not (#{ :core/type, :core/id } k))] k)))

(defn- str-contains?
  [str substr]
  (not= -1 (.indexOf str substr)))

(defn- name-to-str
  [kw]
  (subs (str kw) 1))

(defn- fullName
  [n attr]
  (if (str-contains? (str attr) "/") 
    attr 
    (keyword (str (name-to-str (node-type n)) 
                "/" 
                (name-to-str attr)))))

(defn has-attr?
  [n attr]
  (let [fa (fullName n attr)]
    (contains? n fa)))

(defn node-attr
  [n attr]
  (let [fa (fullName n attr)]
    (do
      (if (not (contains? n fa))   ;; HACK: need a better assert
        (println "no attribute" fa "in node" (node-type n) ";" (node-attrs n)))
      (assert (contains? n fa))
      (n fa))))

(defn node-children
	"List of child nodes (non-node-values ignored), including nodes in list-valued attributes."
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
  
(defn- debug
  [x]
  (do 
    (prn "debug: " x)
    x))

(defn rename-nodes
  "New AST with all nodes assigned new ids. Ref-nodes are updated accordingly,
  with ref-nodes that refer to nodes outside the tree (i.e. free variables) 
  left unchanged."
  ; Would it be easier to build this as a reduction? That would require an 
  ; awkward dep. on reduce.clj (or moving this function there)
  [n]
  (let [ old-to-new-id (reduce merge {} (for [ i (deep-node-ids n) ] { i (genid) })) ]
    (letfn [(map-id [i] (get old-to-new-id i i))
            (rename-node
              [n]
              (cond
                (node? n) 
                (apply node (node-type n) 
                  :core/id
                  (map-id (node-id n))
                  
                  (if (ref-node? n)
                    [:core/ref/id (map-id (ref-node-id n))]
                    (apply concat
                      (for [a (node-attrs n)]
                        [a (rename-node (node-attr n a))]))))
                      
                (vector? n)
                (vec (for [c n] (rename-node c)))
                
                true
                n))]
     (rename-node n))))
  
  
  
; Pretty-printing:
; This is not so pretty, actually, but at least it makes the structure clear.

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
        (let [ kstr (short-attr-name n k)
                v (node-attr n k)]
          (do
            (cond
              (node? v)
              (do
                (println (str indent kstr))
                (print-node v allIds indent))
          
              (vector? v)
              (do
                (println (str indent kstr " ["))
                (dorun
                  (map #(if (node? %) 
                          (print-node % allIds (str indent "  ")) 
                          (println (str indent "  " %))) 
                      v))
                (println (str indent "]")))
          
              (string? v)
              (println (str indent kstr " \"" v "\""))
              
              true
              (println (str indent kstr " " v)))))))
    (println (str indent ")"))))


; File I/O:

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
                      (node? f) 
                        (recur (conj v f))
                      (and (list? f) (= (first f) 'node)) 
                        (recur (conj v (eval f)))
                      true 
                        (recur v)))))]
    (.close r)
    nodes))

(defn load-node
  "Read a single node from a '.mlj' file."
  [fname]
  (first (load-nodes fname)))

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

(deftest node-attr1
  (is (= 1
        (node-attr (node :a :b 1) :a/b)))
  (is (= 1
        (node-attr (node :a :b 1) :b))))

(deftest node-attrs1
  (is (= #{:a/b :a/c}
        (set (node-attrs (node :a :b 1 :c 2))))))

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

(deftest rename1
  (let [ n (node :foo :core/id :1 
            :bar 
            (node :core/ref :id :1)
            :baz 
            (node :core/ref :id :2))
          np (rename-nodes n) ]
    (is (not= (node-id n)
                (node-id np)))
    (is (not= (node-id (node-attr n :foo/bar))
                (node-id (node-attr np :foo/bar))))
    (is (= (node-id np)
          (ref-node-id (node-attr np :foo/bar)))
        "Ref to bound id renamed with it.")
    (is (= (ref-node-id (node-attr n :foo/baz))
          (ref-node-id (node-attr np :foo/baz)))
        "Ref to free id not changed")))
