; Definitions for the constructing and using nodes using a simple concrete
; embedding. These functions encapaulate the encoding of nodes; all other 
; code working with nodes should be based on them to get a measure of 
; abstraction/implementation independence.

; Note: turn this on _before_ the namespace declaration, so it applies to all
; namespaces...
(set! *warn-on-reflection* true)

(ns meta.core
  (:use (clojure set test))
  (:require [clojure.zip :as zip])
  (:import (java.io 
            FileReader
            PushbackReader)))

;
; General utilities:
;
(defmacro assert-pred
  "Evaluates (pred x) and throws an exception if it does not evaluate to
  logical true."
  [pred & more]
  (when *assert*
    `(when-not (~pred ~@more)
       (throw (new AssertionError (str '(~pred ~@more) "; " '~@more " -> " ~@more))))))

(defmacro mapfor
  "Macro for building maps. Takes a list of bindings as in 'for, and 
  expressions for the key and value. Uses a transient map for speed (roughly
  4x faster than the obvious '(reduce merge ...))."
  [bind key val]
  `(persistent! 
    (reduce #(conj! %1 %2) 
            (transient {})
            (for ~bind [~key ~val]))))

(defmacro time2
  "Slightly enhanced time macro"
  [msg expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr
         elapsed# (/ (double (- (. System (nanoTime)) start#)) 1000000.0)]
     (println (str ~msg elapsed# " ms"))
     ret#))


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

(defn- node-value?
  "True if the argument is of the proper type to be the value of a 'value node'."
  [v]
  (or (string? v) 
      (integer? v) 
      (float? v) 
      (keyword? v) 
      (= true v) 
      (= false v)))

(def make-node) ; forward decl.
(def node?)

(defn- wrap-value
  [v]
  (cond
    (node? v)
    v
    
    (string? v)
    (make-node :core/string v)
    
    (integer? v)
    (make-node :core/int v)
    
    (float? v)
    (make-node :core/float v)
    
    (keyword? v)
    (make-node :core/name v)

    (or (= true v) (= false v))
    (make-node :core/boolean v)
    
    true
    (throw (AssertionError. (str "Invalid node value: " v)))))
    ;(assert-pred #(= % 2) v)))

;
; Node type, using the new deftype construct of Clojure 1.2.
;
(deftype nodetype [typ id value]
  Object
  (^boolean equals [^nodetype this ^Object obj]
    (let [^nodetype other obj]
      (and (= (type this) (type obj))
           (= (.typ this) (.typ other))
           ; (= (.id this) (.id obj))  ; HACK: ignore ids for now
           (= (.value this) (.value other)))))
  (toString [^Object this]
    (str "(make-node " (.typ this) " " (.id this) " " (.value this) ")")))

; (println (.getName nodetype))
; (doseq [m (concat (.getDeclaredFields nodetype)
;                   (.getDeclaredMethods nodetype))]
;        (println m))


(defn make-node
  "Constructor for nodes. For convenience, any 'collection' value is converted to 
  a vector (i.e. a seq-node), any primitive value is wrapped in a node
  with the standard type (e.g. :core/string), and any un-qualified attribute 
  name is prefixed with the node type name."
  ([typ]
    (make-node typ (genid) {}))
  ([typ val]
    (make-node typ (genid) val))
  ([typ id val]
    (assert-pred keyword? typ)
    (assert-pred keyword? id)
    (assert-pred #(or (map? %) (vector? %) (seq? %) (node-value? %)) val)
    (if (seq? val) (dorun val))
    (let [v (cond 
              (map? val)
              (mapfor [ [k v] val ] (childname typ k) (wrap-value v)) 
      
              (coll? val)
              (vec (map wrap-value val))
      
              true
              val)]
      (nodetype. typ id v))))

      
(defn node? 
  "True if x represents an AST node (including ref nodes)."
  [x]
  (instance? nodetype x))
  ; (and (vector? x)
  ;     (= (count x) 3)
  ;     (keyword? (x 0))
  ;     (keyword? (x 1))))


(defn node
  "Deprecated: use make-node.
  Build a new node with the given type and children, and a freshly generated 
  id. If a :core/id child is provided, it overrides the generated id."
  [t & children]
  (do
    (assert-pred even? (count children))
    (let [m (mapfor [ [k v] (partition 2 children) ]
                (childname t k) 
                (cond 
                    (node? v)
                    v
                    
                    ; (and (keyword? v) (= :core/id k))
                    ; v
                    
                    ; Simple values will get wrapped in make-node:
                    (node-value? v)
                    v
                    
                    ; Vector needs wrapping; so do its elements, if not nodes
                    (vector? v)
                    (make-node :core/sequence 
                      (vec (map wrap-value v)))
                    
                    true
                    (assert-pred (fn [n] false) v)))
          id (if-let [i (m :core/id)] i (genid))]
      (make-node t id (dissoc m :core/id)))))
      
(defn node-type 
  "The 'type' of the node (a keyword)."
  [#^nodetype n]
  (do
    (assert-pred node? n)
    (.typ n)))
    ; (n 0)))

(defn node-id 
  "The id of the node (a keyword)."
  [#^nodetype n]
  (do
    (assert-pred node? n)
    (.id n)))
    ; (n 1)))

(defn- node-content
  "The content of the node (a map, vector, id (keyword) or simple value)."
  [#^nodetype n]
  (do
    (assert-pred node? n)
    (.value n)))
    ; (n 2)))


(defn map-node?
  [#^nodetype n]
  (map? (node-content n)))
  
(defn seq-node?
  [#^nodetype n]
  (vector? (node-content n)))

(defn ref-node
  "Make a new ref node which points to the node with the given id."
  [id]
  (make-node :core/ref id))
    
(defn ref-node?
  "true if x is a reference-node (that is, a node that represents a pointer to another node)"
  [#^nodetype x]
  (and (node? x) 
        (= (node-type x) :core/ref)))

(defn ref-node-id
  "Id of the node pointed to by the given reference-node."
  [#^nodetype n]
  (do
    (assert-pred ref-node? n)
    (node-content n)))

(defn value-node?
  [#^nodetype n]
  (and (node? n) (node-value? (node-content n))))

(defn node-value
  "Value of a string, int, or keyword node."
  [#^nodetype n]
  (do
    (assert-pred #(and (node? %)
                        (not (or (map-node? %) (seq-node? %)))) n)
    (node-content n)))

(defn node-attrs
	"Seq of attribute/field names/indices."  ; TODO is this useful on sequences?
	[#^nodetype n]
	(do
    (assert-pred node? n)
    (cond
      (map-node? n)
	    (keys (node-content n))
	    
	    (seq-node? n)
	    (range (count (node-content n))))))

(defn- str-contains?
  [s c]
  (not= -1 (.indexOf (str s) (int c))))  ; Note: the signature is String.indexOf(int ch)!

; (defmacro str-contains?
;   [s c]
;   `(not= -1 (.indexOf ~s (int ~c))))  ; Note: the signature is String.indexOf(int ch)!

(defn- name-to-str
  [kw]
  (subs (str kw) 1))


; Optimization: fullName is heavily used and easily memoized. That saves 
; building strings each time an attribute needs to be looked up:
(def fullNameForType
  (memoize
    (fn
      [t attr]
      (if (str-contains? (str attr) \/)
        attr 
        (keyword (str (name-to-str t) 
                      \/
                      (name-to-str attr)))))))
(defn- fullName
  [n attr]
  (fullNameForType (node-type n) attr))

(defn- resolve-name
  [n attr]
  (if (map-node? n)
    (fullName n attr)
    attr))

(defn has-attr?
  [#^nodetype n attr]
  (do 
    (assert-pred #(or (map-node? %) (seq-node? %)) n)
    (contains? (node-content n) (resolve-name n attr))))

(defn node-attr
  [#^nodetype n attr]
  (if (has-attr? n attr)
    ((node-content n) (resolve-name n attr))
    (throw (AssertionError. (str "Attribute not found: " attr " in node " (node-type n) " " (node-attrs n))))))
    
(defn node-attr-value
  [#^nodetype n attr]
  (node-value (node-attr n attr)))

(defn node-children
	"List of child nodes."
	[#^nodetype n]
	(for [a (node-attrs n)] (node-attr n a)))

(defn node-attr-children
  [#^nodetype n attr]
  (node-children (node-attr n attr)))

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
  
  If called with just two args [n, f], then f is a function taking only a node, 
  and the 'env' value is ignored.
  
  With one arg [n], a sequence of all the nodes are simply returned.
  
  TODO: should be lazy?"
  ([n]
    (visitNode n (fn [n] n)))
  ([n f]
    (visitNode n (fn [n _] [(f n) nil]) nil))
  ([#^nodetype n f env]
    (let [ [result newEnv] (f n env)]
      (apply concat
        [result]
        (for [c (node-children n)]
          (visitNode c f newEnv))))))

(defn deep-node-ids
  "Seq of all ids found in the entire tree."
  [#^nodetype n]
  (visitNode n (fn [n env] [(node-id n) env]) nil))
  ; (reduce union #{(node-id n)}
  ;   (for [a (node-attrs n)]
  ;     (let [c (node-attr n a)]
  ;       (cond
  ;         (node? c) (deep-node-ids c)
  ;         (vector? c) (reduce union #{} (for [cn c] (deep-node-ids cn)))
  ;         true #{})))))


(defn find-node
  "Returns the node with the given id, or nil if not found."
  [#^nodetype n id]
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


(defn rename-node
  "Copy of a node with a new id (for just the root)."
  [#^nodetype n]
  ; (println "rename:" (node-type n) (node-id n))  ; HACK
  (make-node (node-type n)
             (genid (subs (str (node-id n) "__") 1))
             (node-content n)))


(defn rename-nodes
  "New AST with all nodes assigned new ids. Ref-nodes are updated accordingly,
  with ref-nodes that refer to nodes outside the tree (i.e. free variables) 
  left unchanged."
  ; Would it be easier to build this as a reduction? That would require an 
  ; awkward dep. on reduce.clj (or moving this function there)
  [#^nodetype n]
  (let [ old-to-new-id (mapfor [ i (deep-node-ids n) ] i (genid (subs (str i "__") 1))) ]
    (letfn [(map-id 
              [i] 
              (get old-to-new-id i i))
            (rename-node
              [n]
              (cond
                (map-node? n) 
                (make-node (node-type n) 
                           (map-id (node-id n))
                           (mapfor [a (node-attrs n)]
                                a (rename-node (node-attr n a))))
                      
                (seq-node? n)
                (make-node (node-type n) 
                           (map-id (node-id n))
                           (vec (for [c (node-children n)] (rename-node c))))
                
                (ref-node? n)
                (ref-node (map-id (ref-node-id n)))
                
                (node? n)
                (make-node (node-type n) 
                           (map-id (node-id n))
                           (node-value n))))]
     (rename-node n))))
  
  
  
; Pretty-printing:
; This is not so pretty, actually, but at least it makes the structure clear.

(defn short-attr-name
  [#^nodetype n a]
  (let [nstr (str (node-type n))
        kstr (str a)]
    (if (.startsWith kstr nstr) 
      (str ":" (subs kstr (inc (count nstr)))) 
      kstr)))

(defn print-node
  "Prints a parsable (and vaguely human-readable) representation of the given
  node. Note: it's effectively a pretty-printed program using the (make-node) fn."
  ([#^nodetype n]
    (print-node n false))
  ([#^nodetype n allIds]
    (print-node n allIds ""))
  ([#^nodetype n allIds indent]
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
      ; (assert false))))
      (println (str indent "??? " n)))))


; File I/O:

(defn load-nodes
  "Read nodes from a '.mlj' file. Nodes can be in raw form (i.e. 3-vectors) or as 
  Clojure forms which evaluate to the actual nodes (i.e. '(make-node :foo ...)').
  Any form which isn't a node and isn't a (make-node ...) or (node ...) form is 
  ignored."
  [fname]
  (let [r (PushbackReader. (FileReader. (str fname)))
        nodes (loop [ v [] ]
                (let [ f (read r false :eof) ]
                  (if (= f :eof)
                    v
                    (cond 
                      (node? f) 
                        (recur (conj v f))
                      (and (list? f) (contains? #{'node 'make-node} (first f))) 
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

