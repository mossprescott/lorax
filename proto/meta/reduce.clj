; Support for syntax extension

(ns meta.reduce
  (:use (clojure test))
  (:use (meta core)))
  
(def meta-reduce) ; forward-decl!

(defn- meta-reduce-children
  [n f]
    ; (do (print "reducing children: ") (println n) ; HACK
    (apply hash-map 
     (apply concat 
        (for [ [ k v ] (seq n) ]
          ; (do (println "k:" k) ; HACK
          [ k (cond
                (node? v) (meta-reduce v f)
                (vector? v) (vec (map #(meta-reduce % f) v))
                true v)]))))
          ; ) ; HACK


(defn meta-reduce
  "Evaluate any syntax extensions according to the given rules (macros).
  For instance, the rules might reduce the full 'Clojure' syntax to the 
  Clojure kernel language, which can then be evaluated by direct translation 
  to raw Clojure forms.
  f takes a node and produces a reduced node (which is then subject to 
  continued reduction), or nil (meaning the node cannot be reduced).
  Once the top-level node is no longer reducible, it is 
  simply rebuilt with its recursively reduced children. A more intelligent 
  approach might be to check each fully-reduced node against the expected 
  grammar."
  [n f]
  ; (do (print "reducing: ") (println n) ; HACK
  (let [ np (f n) ]
    ; (print "from: ") (print-node n)
    ; (print "to: ")(if (node? np) (print-node np) (println np))
    (if (nil? np)
      ; No reduction occurred, so don't loop!
      (meta-reduce-children n f) 
      ; Note: need to continue reducing the result, which might contain more syntax
      (recur np f))))
  ; ) ; HACK

  ; (meta-reduce-children n f))
    ; ) ; HACK
  ; ) ; HACK

; ===========================================================================
; Fancier reduction, keeping track of what source node gave rise to each node
; of the result.
; ===========================================================================

(def PRINT false)

(def meta-reduce-one2) ; forward-decl!

(defn- meta-reduce-child2
  [v f]
  ;(println "v" v)
  (cond
    (node? v) 
    (meta-reduce-one2 v f)
    
    (vector? v) 
    (let [ts (map #(meta-reduce-one2 % f) v)]
      [(vec (map first ts))
        (reduce merge {} (map second ts))])
    
    true 
    [v {}]))

(defn- meta-reduce-children2
  "Reduce the children of a node, returning a node with the same id and a map
  of descendant node ids to the original node id for each."
  [n f]
  ; (println "n" n)
  (let [ childrenAndMaps (for [ [ k v ] (seq n) ]
                          (let [ [ rc o ] (meta-reduce-child2 v f)]
                            [ k rc o ]))
        ; forced (doall childrenAndMaps)
        ; foo (println "chAM" childrenAndMaps)
        reducedNode (reduce #(assoc %1 (first %2) (second %2)) {} childrenAndMaps)
        origins (reduce #(merge %1 (nth %2 2)) {} childrenAndMaps)]
    [ reducedNode origins ]))


(defn- meta-reduce-one2
  "Reduce, producing a map from ids of nodes in the result to the ids of the nodes
  they were reduced from. Only the root of each reduced sub-tree is tracked. Non-reduced 
  nodes' ids are mapped to themselves.
  This means that the smallest original-program node for any reduced node can be found 
  by looking up the node and its ancestors in order.
  The resulting map contains the ids of all resulting nodes, whether or not they appear
  in the 'original' program."
  [n f]
  (if (node? n)
    (let [ _ (if PRINT (print-node n))
            origId (node-id n)
            np (f n) ]
      ; (println "origId" origId)
      ; (println "np" np)
      (let [ [ npp o ] (if (nil? np)
                          (meta-reduce-children2 n f)  ; n is fully-reduced; recursively reduce its children
                          (meta-reduce-one2 np f)) ; n may need additional reduction
              op (if (node? npp)
                  (assoc o (node-id npp) origId) ; this includes all nodes in the result, mapping new nodes to themselves
                  o) ] ; if the result is not a node (e.g. it's a vector) then the id mapping is lost
              ; [;op (if (nil? np) o (assoc o (node-id npp) origId)) ]
          [ npp op ]))
    [ n {} ]))

(defn- submap
  "A new map containing mappings from m for only the keys in ks."
  [m ks]
  (reduce merge {} (for [k (keys m) :when (ks k)] {k (m k)})))

(deftest submap-empty
  (is (= {} (submap {} #{}))))

(deftest submap-half
  (is (= {:a 1} (submap {:a 1, :b 2} #{:a}))))

(defn- valuesubmap
  "A new map containing mappings from m for only the values in vs."
  [m vs]
  (reduce merge {} (for [k (keys m) :when (vs (m k))] {k (m k)})))

(deftest valuesubmap-empty
  (is (= {} (submap {} #{}))))

(deftest valuesubmap-half
  (is (= {:a 1} (valuesubmap {:a 1, :b 2} #{1}))))

(defn meta-reduce2
  "Reduce, producing a map from ids of nodes in the result to the ids of the nodes
  they were reduced from. Only the root of each reduced sub-tree is tracked. Non-reduced 
  nodes' ids are mapped to themselves.
  This means that the smallest original-program node for any reduced node can be found 
  by looking up the node and its ancestors in order.
  Currently works by filtering the result of meta-reduce-one2 to include mappings for
  nodes in the original program only. This isn't particulary clever or efficient."
  [n f]
  (let [origIds (set (deep-node-ids n))
        [np o] (meta-reduce-one2 n f)]
    [np (valuesubmap o origIds)]))
    

(defn reduceByType
  "Reduction fxn built from a map of node types to fxns."
  ; TODO: accept multiple maps (and merge them?)
  [rules]
  (fn [n] 
    (if-let [ f (rules (node-type n)) ]
      (f n)
      nil)))


(defmacro with-attr
  "Macro which binds _c_ to the value of an attribute, if present, and then 
  evaluates _body_ (which can refer to _c_). Otherwise, _missing_ is evaluated."
  [n attrName c body missing]
  `(if-let [~c (~n ~attrName)]
      ~body
      ~missing))

(defmacro with-attr-node
  "With 2 arguments, returns either the value of the attribute or a 'missing'
  node. With 4 arguments, evaluates _body_ if the attribute is found."
  ; TODO: some more validation of the child?
  ([n attrName]
    `(with-attr-node ~n ~attrName c# c#))
  ([n attrName c body]
    `(with-attr ~n ~attrName ~c ~body 
      (node :view/expr/missing))))
  
(defmacro with-attr-seq
  "With 2 arguments, returns either the value of the attribute or a sequence
  containing only a 'missing' node. With 4 arguments, evaluates _body_ if the 
  attribute is found."
  ; TODO: some more validation of the child?
  ([n attrName]
    `(with-attr-seq ~n ~attrName c# c#))
  ([n attrName c body]
    `(with-attr ~n ~attrName ~c ~body 
      [(node :view/expr/missing)])))
  

(defn apply-until
  "Given a collection of functions, a function which applies each fxn to its 
  argument in turn, returning the first non-nil result."
  [fxns]
  (fn [n]
    (loop [s (seq fxns)]
      (if-let [f (first s)]
        (if-let [np (f n)]
          np
          (recur (rest s)))
        nil))))

;
; Tests:
;

(deftest unreduced
  (let [n1 (node :foo :core/id :1)]
    (is (= (meta-reduce2 n1 (fn [n] nil))
            [n1 {:1 :1}]))))
      
(deftest reduced
  (let [n1 (node :foo :core/id :1)
        n2 (node :bar :core/id :2)
        r (fn [n] (if (= (node-type n) :foo) n2 nil)) ]
    (is (= (meta-reduce2 n1 r)
            [n2 {:2 :1}])
      "A single reduced node's id should appear in the result")))
      
(deftest semi-reduced
  (let [n1 (node :foo :core/id :1)
        n2 (node :bar :core/id :2)
        n3 (node :baz :core/id :3 :child n1)
        n4 (node :baz :core/id :3 :child n2)
        r (fn [n] (if (= (node-type n) :foo) n2 nil)) ]
    (is (= (meta-reduce2 n3 r)
            [n4 {:2 :1, :3 :3}])
      "One reduced and one not.")))
      
(deftest semi-vec
  (let [n1 (node :foo :core/id :1)
        n2 (node :bar :core/id :2)
        n3 (node :baz :core/id :3 :children [ n1 ])
        n4 (node :baz :core/id :3 :children [ n2 ])
        r (fn [n] (if (= (node-type n) :foo) n2 nil)) ]
    (is (= (meta-reduce2 n3 r)
            [n4 {:2 :1, :3 :3}])
      "One reduced and one not, in a vector.")))
      
(deftest node-to-vec
  (let [n1 (node :foo :core/id :1)
        n2 (node :bar :core/id :2)
        n3 (node :baz :core/id :3 :children n1)
        n4 (node :baz :core/id :3 :children [ n2 ])
        r (fn [n] (if (= (node-type n) :foo) [ n2 ] nil)) ]
    (is (= (meta-reduce2 n3 r)
            [n4 {:3 :3}])  ; TODO
      "A node is reduced to a vector (and the original id is lost in the process for now).")))
      
(deftest introduced
  (let [n1 (node :foo :core/id :1)
        n3 (node :bleep)
        n2 (node :bar :core/id :2 :other n3)
        n4 (node :baz :core/id :3 :child n1)
        n5 (node :baz :core/id :3 :child n2)
        r (fn [n] (if (= (node-type n) :foo) n2 nil)) 
        [np o] (meta-reduce2 n4 r)]
    (is (= np n5))
    (is (= o {:2 :1, :3 :3})
      "A node introduced by a reduction should not appear in the result map.")))

(deftest apply-until1
  (let [n (fn [n] nil)
        i (fn [n] n)]
    (is (= ((apply-until []) 1)
          nil)
        "none")
    (is (= ((apply-until [n]) 1)
          nil)
        "just nil")
    (is (= ((apply-until [i]) 1)
          1)
        "just id")
    (is (= ((apply-until [n i]) 1)
          1)
        "nil first")
    (is (= ((apply-until [i n]) 1)
          1)
        "id first")
    (is (= ((apply-until [n n n n n n n n]) 1)
          nil)
        "lotta nil")
    (is (= ((apply-until [n n n n n n n n i]) 1)
          1)
        "eventually id")))
        
(deftest with-attr1
  (is (= (with-attr (node :foo) :foo/bar bar (node :baz :quux bar) 1)
          1)
          "missing")
  (is (= (with-attr (node :foo :bar 2) :foo/bar bar (node :baz :core/id :1 :quux bar) 1)
          (node :baz :core/id :1 :quux 2))
          "present"))
