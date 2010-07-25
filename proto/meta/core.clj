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


(defmacro assert-pred
  "Evaluates (pred x) and throws an exception if it does not evaluate to
  logical true."
  [pred & more]
  (when *assert*
    `(when-not (~pred ~@more)
       (throw (new AssertionError (str '(~pred ~@more) "; " '~@more " -> " ~@more))))))

;
; Some utilities for working with nodes:
;
(defn genid
  "A new, globally unique node id."
  ([]
    (genid "__"))
  ([s]
    (keyword (str (gensym s)))))

(defn- kwname 
  [kw]
  (subs (str kw) 1))
  
(defn- childname 
  [t kw]
  (if (not= (.indexOf (str kw) (int \/)) -1)
    kw
    (keyword (str (kwname t) "/" (kwname kw)))))

; TODO: use struct-map for nodes? If there was a way to test that an object 
; was an instance of the struct map, that would give some "type safety".
; It's not clear that it would be any more efficient than a small vector.

(defn node-value?
  "True if the argument is of the proper type to be the value of a 'value node'."
  [v]
  (or (string? v) 
      (integer? v) 
      (float? v) 
      (keyword? v) 
      (= true v) 
      (= false v)))

(defn make-node
  "Constructor for nodes."
  ([typ val]
    (make-node typ (genid) val))
  ([typ id val]
    (assert-pred keyword? typ)
    (assert-pred keyword? id)
    (assert-pred #(or (map? %) (vector? %) (node-value? %)) val)
    (if (map? val) 
      [typ id (reduce merge {} (for [ [k v] val ] { (childname typ k) v }))]
      [typ id val])))

      
(defn node? 
  "True if x represents an AST node (including ref nodes)."
  [x]
  (and (vector? x)
      (= (count x) 3)
      (keyword? (x 0))
      (keyword? (x 1))))


(defn node
  "Deprecated: use make-node.
  Build a new node with the given type and children, and a freshly generated 
  id. If a :core/id child is provided, it overrides the generated id."
  [t & children]
  (do 
    (assert-pred even? (count children))
    (let [m (reduce merge {}
              (for [ [k v] (partition 2 children) ]
                { (childname t k) 
                  (cond 
                    (node? v)
                    v
                    
                    (and (keyword? v) (= :core/id k))
                    v
                    
                    ; Simple values need wrapping
                    (node-value? v)
                    (make-node :anonymous v)
                    
                    ; Vector needs wrapping; so do its elements, if not nodes
                    (vector? v)
                    (make-node :anonymous 
                      (vec (for [c v] (if (node? c) c (make-node :anonymous c)))))
                    
                    true
                    (assert-pred (fn [n] false) v)) }))
          id (if-let [i (m :core/id)] i (genid))]
      (make-node t id (dissoc m :core/id)))))
      
(defn node-type 
  "The 'type' of the node (a keyword)."
  [n]
  (do
    (assert-pred node? n)
    (n 0)))

(defn node-id 
  "The id of the node (a keyword)."
  [n]
  (do
    (assert-pred node? n)
    (n 1)))

(defn- node-content
  "The content of the node (a map, vector, id (keyword) or simple value)."
  [n]
  (do
    (assert-pred node? n)
    (n 2)))


(defn map-node?
  [n]
  (map? (node-content n)))
  
(defn seq-node?
  [n]
  (vector? (node-content n)))

(defn ref-node
  "Make a new ref node which points to the node with the given id."
  [id]
  (make-node :core/ref id))
    
(defn ref-node?
  "true if x is a reference-node (that is, a node that represents a pointer to another node)"
  [x]
  (and (node? x) 
        (= (node-type x) :core/ref)))

(defn ref-node-id
  "Id of the node pointed to by the given reference-node."
  [n]
  (do
    (assert-pred ref-node? n)
    (node-content n)))

(defn node-value
  "Value of a string, int, or keyword node."
  [n]
  (do
    (assert-pred #(and (node? %)
                        (not (or (map-node? %) (seq-node? %)))) n)
    (node-content n)))

(defn node-attrs
	"Seq of attribute/field names/indices."  ; TODO is this useful on sequences?
	[n]
	(do
    (assert-pred node? n)
    (cond
      (map-node? n)
	    (keys (node-content n))
	    
	    (seq-node? n)
	    (range (count (node-content n))))))

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

(defn- resolve-name
  [n attr]
  (if (map-node? n)
    (fullName n attr)
    attr))

(defn has-attr?
  [n attr]
  (do 
    (assert-pred #(or (map-node? %) (seq-node? %)) n)
    (contains? (node-content n) (resolve-name n attr))))

(defn node-attr
  [n attr]
  (do
    (assert-pred has-attr? n attr)
    ((node-content n) (resolve-name n attr))))

(defn node-children
	"List of child nodes."
	[n]
	(for [a (node-attrs n)] (node-attr n a)))

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
  (but more or less in-order).
  
  TODO: should be lazy?"
  [n f env]
  (let [ [result newEnv] (f n env)]
    (apply concat
      [result]
      (for [c (node-children n)]
        (visitNode c f newEnv)))))

(defn deep-node-ids
  "Seq of all ids found in the entire tree."
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
  
; (defn- debug
;   [x]
;   (do 
;     (prn "debug: " x)
;     x))

(defn rename-nodes
  "New AST with all nodes assigned new ids. Ref-nodes are updated accordingly,
  with ref-nodes that refer to nodes outside the tree (i.e. free variables) 
  left unchanged."
  ; Would it be easier to build this as a reduction? That would require an 
  ; awkward dep. on reduce.clj (or moving this function there)
  [n]
  (let [ old-to-new-id (reduce merge {} (for [ i (deep-node-ids n) ] { i (genid) })) ]
    (letfn [(map-id 
              [i] 
              (get old-to-new-id i i))
            (rename-node
              [n]
              (cond
                (map-node? n) 
                (make-node (node-type n) 
                           (map-id (node-id n))
                           (reduce merge {}
                              (for [a (node-attrs n)]
                                {a (rename-node (node-attr n a))})))
                      
                (seq-node? n)
                (make-node (node-type n) 
                           (map-id (node-id n))
                           (vec (for [c (node-children n)] (rename-node c))))
                
                (ref-node? n)
                (ref-node (map-id (ref-node-id n)))
                
                (node?)
                (make-node (node-type n) 
                           (map-id (node-id n))
                           (node-value n))))]
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
  node. Note: it's effectively a pretty-printed program using the (make-node) fn."
  ([n]
    (print-node n false))
  ([n allIds]
    (print-node n allIds ""))
  ([n allIds indent]
    ; TODO: handle ref-node
    (cond
      (node? n)
      (do
        ; prefix and node type:
        (print (str indent "(make-node " (node-type n) " "))
      
        ; id, if desired:
        (if (or allIds (not (.startsWith (str (node-id n)) ":_")))
              (print (str (node-id n) " ")))

        ; value:
        (cond
          (map-node? n)
          (if (empty? (node-attrs n))
            (print "{}")
            (do
              (println "{")
              (doseq [a (node-attrs n)] 
                (println (str indent "  " (short-attr-name n a)))
                (print-node (node-attr n a) allIds (str indent "  ")))
              (print (str indent "}"))))
        
        
          (seq-node? n)
          (do
            (println "[")
            (doseq [c (node-children n)] (print-node c allIds (str indent "  ")))
            (print (str indent "]")))
                
          true
          (pr (node-value n)))
      
        (println ")"))
      
      true
      (println (str indent "??? " n)))))
        
      ; (node? n)
      ; (do
      ;   (println (str indent "(node " (node-type n))) 
      ;   (let [ indent (str "  " indent) ]
      ;     (if (or allIds (not (.startsWith (str (node-id n)) ":_")))
      ;       (println (str indent ":core/id " (node-id n))))
      ;     (doseq [ k (keys n) :when (not (#{:core/type :core/id} k)) ]
      ;       (let [ kstr (short-attr-name n k)
      ;               v (node-attr n k)]
      ;         (do
      ;           (cond
      ;             (node? v)
      ;             (do
      ;               (println (str indent kstr))
      ;               (print-node v allIds indent))
      ;     
      ;             (vector? v)
      ;             (do
      ;               (println (str indent kstr " ["))
      ;               (dorun
      ;                 (map #(if (node? %) 
      ;                         (print-node % allIds (str indent "  ")) 
      ;                         (println (str indent "  " %))) 
      ;                     v))
      ;               (println (str indent "]")))
      ;     
      ;             (string? v)
      ;             (println (str indent kstr " \"" v "\""))
      ;         
      ;             true
      ;             (println (str indent kstr " " v)))))))
      ;   (println (str indent ")"))))))


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
; Tests:
;
(deftest node-value1
  (is (= 1 
         (node-value (make-node :foo 1))))
  (is (= "abc"
         (node-value (make-node :foo "abc")))))

(deftest node-attr1
  (is (= 1
        (node-value (node-attr (node :a :b 1) :a/b))))
  (is (= 1
        (node-value (node-attr (node :a :b 1) :b)))))

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
  (is (= #{:1 :2 :3 :4}
        (set (deep-node-ids 
              (make-node :foo :1 { 
                :child 
                (make-node :seq :4 [
                  (make-node :bar :2 {})
                  (make-node :baz :3 {})
                ])
              }))))))

(deftest rename1
  (let [ n (make-node :foo :1 { 
            :bar 
            (ref-node :1)  ; bound
            :baz 
            (ref-node :2)})  ; free
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

;
; New representation:
;
(deftest make-node1
  (is (node? (make-node :foo {})))
  (is (node? (make-node :foo :1 {})))
  (is (node? (make-node :foo :1 [])))
  (is (node? (make-node :foo :1 "abc")))
  (is (node? (make-node :foo :1 1))))

