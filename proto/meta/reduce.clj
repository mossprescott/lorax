; Support for syntax extension

(ns meta.reduce
  (:use (clojure test))
  (:use (meta core)))
  
; (def meta-reduce) ; forward-decl!

; (defn- meta-reduce-children
;   [n f]
;     ; (do (print "reducing children: ") (println n) ; HACK
;     (apply hash-map 
;      (apply concat 
;         (for [ [ k v ] (seq n) ]
;           ; (do (println "k:" k) ; HACK
;           [ k (cond
;                 (node? v) (meta-reduce v f)
;                 (vector? v) (vec (map #(meta-reduce % f) v))
;                 true v)]))))
;           ; ) ; HACK
; 
; 
; (defn meta-reduce
;   "Evaluate any syntax extensions according to the given rules (macros).
;   For instance, the rules might reduce the full 'Clojure' syntax to the 
;   Clojure kernel language, which can then be evaluated by direct translation 
;   to raw Clojure forms.
;   f takes a node and produces a reduced node (which is then subject to 
;   continued reduction), or nil (meaning the node cannot be reduced).
;   Once the top-level node is no longer reducible, it is 
;   simply rebuilt with its recursively reduced children. A more intelligent 
;   approach might be to check each fully-reduced node against the expected 
;   grammar."
;   [n f]
;   ; (do (print "reducing: ") (println n) ; HACK
;   (let [ np (f n) ]
;     ; (print "from: ") (print-node n)
;     ; (print "to: ")(if (node? np) (print-node np) (println np))
;     (if (nil? np)
;       ; No reduction occurred, so don't loop!
;       (meta-reduce-children n f) 
;       ; Note: need to continue reducing the result, which might contain more syntax
;       (recur np f))))
;   ; ) ; HACK
; 
;   ; (meta-reduce-children n f))
;     ; ) ; HACK
;   ; ) ; HACK


; ===========================================================================
; Fancier reduction, keeping track of what source node gave rise to each node
; of the result.
; ===========================================================================

(def PRINT false)
(def PRINT_UNREDUCED_TYPES false)
(def PRINT_REDUCED_TYPES false)

(def meta-reduce-one2) ; forward-decl!

; (defn- meta-reduce-child2
;   [v f continue]
;   ;(println "v" v)
;   (cond
;     (node? v) 
;     (meta-reduce-one2 v f continue)
;     
;     (vector? v) 
;     (let [;_ (println "v: " v)
;           ts (map #(meta-reduce-one2 % f continue) v)
;           ; _ (println "ts:" ts)
;           ]
;       [(vec (map first ts))
;         (reduce merge {} (map second ts))])
;     
;     true 
;     [v {}]))
; 
; (defn- meta-reduce-children2
;   "Reduce the children of a node, returning a node with the same id and a map
;   of descendant node ids to the original node id for each."
;   [n f continue]
;   ; (println "n" n)
;   (let [ childrenAndMaps (for [ [ k v ] (seq n) ]
;                           (let [ [ rc o ] (meta-reduce-child2 v f continue)]
;                             [ k rc o ]))
;         ; forced (doall childrenAndMaps)
;         ; foo (println "chAM" childrenAndMaps)
;         reducedNode (reduce #(assoc %1 (first %2) (second %2)) {} childrenAndMaps)
;         origins (reduce #(merge %1 (nth %2 2)) {} childrenAndMaps)]
;     [ reducedNode origins ]))
; 
; 
; (defn- meta-reduce-one2
;   "Reduce, producing a map from ids of nodes in the result to the ids of the nodes
;   they were reduced from. Only the root of each reduced sub-tree is tracked. Non-reduced 
;   nodes' ids are mapped to themselves.
;   If _continue_ is true, then the resulting node is repeatedly reduced until 
;   the reduction fxn returns nil. Otherwise each source program node is reduced 
;   exactly once if at all.
;   This means that the smallest original-program node for any reduced node can be found 
;   by looking up the node and its ancestors in order.
;   The resulting map contains the ids of all resulting nodes, whether or not they appear
;   in the 'original' program."
;   [n f continue]
;   (if (node? n)
;     (let [ ;_ (if PRINT (println "f:" f))
;             _ (if PRINT (do (print "reduce-one: ")(print-node n true)))
;             origId (node-id n)
;             np (f n) ]
;       ; (println "origId" origId)
;       ; (println "np" np)
;       (let [ [ npp o ] (cond 
;                           (nil? np) (meta-reduce-children2 n f continue)  ; n is fully-reduced; recursively reduce its children
;                           (not continue) (meta-reduce-children2 np f continue)  ; do not attempt to reduce new node
;                           (vector? np) (meta-reduce-child2 np f continue)  ; Tricky! if a node was reduced to a vector, its contents may need reduction
;                           true (meta-reduce-one2 np f continue)) ; n may need additional reduction
;               op (if (node? npp)
;                   (assoc o (node-id npp) origId) ; this includes all nodes in the result, mapping new nodes to themselves
;                   o) ] ; if the result is not a node (e.g. it's a vector) then the id mapping is lost
;               ; [;op (if (nil? np) o (assoc o (node-id npp) origId)) ]
;           [ npp op ]))
;     [ n {} ]))

(defn- submap
  "A new map containing mappings from m for only the keys in ks."
  [m ks]
  (mapfor [k (keys m) :when (ks k)] k (m k)))

(deftest submap-empty
  (is (= {} (submap {} #{}))))

(deftest submap-half
  (is (= {:a 1} (submap {:a 1, :b 2} #{:a}))))

(defn- valuesubmap
  "A new map containing mappings from m for only the values in vs."
  [m vs]
  (mapfor [k (keys m) :when (vs (m k))] k (m k)))

(deftest valuesubmap-empty
  (is (= {} (submap {} #{}))))

(deftest valuesubmap-half
  (is (= {:a 1} (valuesubmap {:a 1, :b 2} #{1}))))


(def reduce-plus)  ; forward decl.!

(defn meta-reduce2
  "Reduce, producing a map from ids of nodes in the result to the ids of the nodes
  they were reduced from. Only the root of each reduced sub-tree is tracked. Non-reduced 
  nodes' ids are mapped to themselves.
  This means that the smallest original-program node for any reduced node can be found 
  by looking up the node and its ancestors in order.
  Currently works by filtering the result of meta-reduce-one2 to include mappings for
  nodes in the original program only. This isn't particulary clever or efficient."
  [#^meta.core.nodetype n f]
  (let [fp (fn [n v] [(f n) nil])
        [np o vp] (reduce-plus n fp nil)]
    [np o]))
  ; (let [origIds (set (deep-node-ids n))
  ;       [np o] (meta-reduce-one2 n f true)]
  ;     [np (valuesubmap o origIds)]))

(defn meta-reduce
  "Reduce, returning only the resulting node and no map of ids."
  [n f]
  (let [[np o] (meta-reduce2 n f)]
    np))

(defn reduceByType
  "Reduction fxn built from a map of node types to fxns."
  ; TODO: accept multiple maps (and merge them?)
  [rules]
  (fn [#^meta.core.nodetype n] 
    (if-let [ f (rules (node-type n)) ]
      (f n)
      nil)))


;
; Augmented reduction, which threads some kind of value through the reduction.
; There is some relationship between this and an _attribute_grammar_, but it's
; not entirely clear.
;

(def reduce-one-plus) ; forward-decl!

; (defn- reduce-child-plus
;   ;; TODO: remove this? Only nodes ever make it here now, I think.
;   [c f v depth]
;   ; (prn "c:" c)  ; HACK
;   (cond
;     (node? c) 
;     (reduce-one-plus c f v depth)
;     
;     ; (vector? c) 
;     ; (let [; _ (println "c: " c)
;     ;       ts (map #(reduce-one-plus % f v depth) c)  ; TODO: thread v through the reductions
;     ;       ; _ (println "ts:" ts)
;     ;       ]
;     ;   [(vec (map first ts))
;     ;     (reduce merge {} (map second ts))
;     ;     v])  ; TODO: return last v
;     
;     true 
;     (do
;       (println "Never happens?")
;       [c {} v])))

(defn merge!
  "Merge one or more maps into a transient map (the first arg)."
  ([tm m1]
    (loop [tm tm es (seq m1)]
              (if es
                (recur (conj! tm (first es))
                       (next es))
                tm)))
  ([tm m1 m2 & more]
    (apply merge! (merge! tm m1) m2 more)))

(defn- reduce-children-plus
  "Reduce the children of a node, returning a node (with the same id and set 
  of attributes) and a map of descendant node ids to the original node id for 
  each."
  ; TODO: detect when no reduction has taken place and return the original node?
  ; As it is, each node gets rebuilt, which means allocating an entire new tree
  ; on every reduction. Is that why it's slow?
  [#^meta.core.nodetype n f v depth]
  (cond 
    (map-node? n)
    (let [ childrenAndMaps (for [ a (node-attrs n) ]
                            (let [ c (node-attr n a)
                                   [ rc o vp ] (reduce-one-plus c f v depth)  ; TODO: thread the value through?
                                   ]
                              [ a rc o vp ]))
          ; val (reduce (fn [ m [a c o vp] ] (assoc m a c)) 
          ;             {} childrenAndMaps)
          val (mapfor [[a c o vp] childrenAndMaps] a c)
          reducedNode (make-node (node-type n)
                                 (node-id n)
                                 val)
          ; origins (reduce (fn [ m [a c o vp]] (merge m o))
          ;                   {} childrenAndMaps)
          origins (persistent!
                    (reduce (fn [m [a c o vp]] (merge! m o))
                                (transient {}) childrenAndMaps))
          ]
      [ reducedNode origins v ])
    
    (seq-node? n)
    (let [childrenAndMaps (for [c (node-children n)]
                            (reduce-one-plus c f v depth))
          val (vec (for [ [c o vp] childrenAndMaps ] c))
          
          reducedNode (make-node (node-type n)
                                 (node-id n)
                                 val)
          ; origins (reduce (fn [ m [c o vp]] (merge m o))
          ;                   {} childrenAndMaps)
          origins (persistent!
                    (reduce (fn [m [c o vp]] (merge! m o))
                                (transient {}) childrenAndMaps))
          ]
      [ reducedNode origins v ])
    
    true
    [n {} v]))

(defn- indent
  [depth] 
  (apply str (repeat depth "  ")))  ; an easier, more efficient way?

(defn- reduce-one-plus
  "Reduce, producing a map from ids of nodes in the result to the ids of the nodes
  they were reduced from. Only the root of each reduced sub-tree is tracked. Non-reduced 
  nodes' ids are mapped to themselves.
  This means that the smallest original-program node for any reduced node can be found 
  by looking up the node and its ancestors in order.
  The resulting map contains the ids of all resulting nodes, whether or not they appear
  in the 'original' program."
  [#^meta.core.nodetype n f v depth]
  (if (node? n)
    (let [_ (if PRINT (do (print "reduce-one: ")(print-node n true)))
          origId (node-id n)
          [np vp] (f n v) ]  ; apply the reduction!
      (if (nil? np)
        (if PRINT_UNREDUCED_TYPES
          (println (indent depth) (node-type n)))
        (if PRINT_REDUCED_TYPES
          (println (indent depth) (node-type n) (str "-> " (node-type np)))))
      (let [ [ npp o vpp] (cond 
                            (nil? np) (reduce-children-plus n f vp (inc depth))  ; n is fully-reduced; recursively reduce its children
                            true (reduce-one-plus np f vp (inc depth))) ; n may need additional reduction
              op (if (node? npp)
                  (assoc o (node-id npp) origId) ; this includes all nodes in the result, mapping new nodes to themselves
                  o) ; if the result is not a node (e.g. it's a vector) then the id mapping is lost
              ; [;op (if (nil? np) o (assoc o (node-id npp) origId)) ]
              ]
          [ npp op vpp ]))
    [ n {} v ]))


(defn reduce-plus
  "Reduce, producing a possibly-reduced node, a map of ids as in meta-reduce-2,
  and a value which has been threaded through the reduction.
  The reduction function takes a node and value, and returns a vector of
  a reduced node (or nil) and a new value."
  [#^meta.core.nodetype n f v]
  (do
    (if PRINT_REDUCED_TYPES (println "\nreduce-plus:" (node-type n)))
    (let [origIds (set (deep-node-ids n))
          [np o vp] (reduce-one-plus n f v 0)]
      [np (valuesubmap o origIds) vp])))


(defn reduceByType-plus
  "Reduction fxn built from a map of node types to fxns."
  ; TODO: accept multiple maps (and merge them?)
  [rules]
  (fn [n v] 
    (if-let [ f (rules (node-type n)) ]
      (f n v)
      [nil v])))

(defn compose-reductions
  "Given a list of reduction fxns (node -> [reduced-node { reduced-id to source-id }]),
  returns a function applying them in turn."
  ([r1 r2]
    (fn [n]
      (let [ [n1 o1] (r1 n)
             [n2 o2] (r2 n1)
             o (mapfor [ [r s] o2 ] r (o1 s)) ]
        [n2 o])))
  ([r1 r2 & more]
    (compose-reductions r1 (apply compose-reductions r2 more))))

;
; Safe attribute accessors, which return some sort of default if the attribute 
; doesn't match:
;

(defmacro with-attr
  "Macro which binds _c_ to the value of an attribute, if present, and then 
  evaluates _body_ (which can refer to _c_). Otherwise, _missing_ is evaluated."
  [n attrName c body missing]
  `(if (has-attr? ~n ~attrName)  ; TODO: temps to avoid re-evaluating n and attrName?
      (let [~c (node-attr ~n ~attrName)]
        ~body)
      ~missing))
  ; `(if-let [~c (~n ~attrName)]
  ;     ~body
  ;     ~missing))

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
  containing only a 'missing' node. With 4 arguments, binds _c_ to the value 
  and evaluates _body_ if the attribute is found."
  ; TODO: some more validation of the child?
  ([n attrName]
    `(with-attr-seq ~n ~attrName c# c#))
  ([n attrName c body]
    `(with-attr ~n ~attrName ~c ~body 
      (make-node :core/sequence [ (make-node :view/expr/missing {}) ]))))
  

(defn apply-until
  "Given a collection of functions, a function which applies each fxn to its 
  argument in turn, returning the first non-nil result. Might be useful for 
  chaining reductions."
  [& fxns]
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
  (let [n1 (make-node :foo :1 {})]
    (is (= (meta-reduce2 n1 (fn [n] nil))
            [n1 {:1 :1}]))))
      
(deftest reduced
  (let [n1 (make-node :foo :1 {})
        n2 (make-node :bar :2 {})
        r (fn [n] (if (= (node-type n) :foo) n2 nil)) ]
    (is (= (meta-reduce2 n1 r)
            [n2 {:2 :1}])
      "A single reduced node's id should appear in the result")))
      
(deftest semi-reduced
  (let [n2 (make-node :bar :2 {})
        n3 (make-node :baz :3 { 
                :child 
                (make-node :foo :1 {}) 
              })
        n4 (make-node :baz :3 { 
                :child 
                n2
              })
        r (fn [n] (if (= (node-type n) :foo) n2 nil)) ]
    (is (= (meta-reduce2 n3 r)
            [n4 {:2 :1, :3 :3}])
      "One reduced and one not.")))
      
(deftest semi-vec
  (let [n1 (make-node :foo :1 {})
        n2 (make-node :bar :2 {})
        n3 (make-node :baz :3 {
              :children 
              (make-node :quux :4 [ n1 ])
            })
        n4 (make-node :baz :3 {
              :children 
              (make-node :quux :4 [ n2 ])
            })
        r (fn [n] (if (= (node-type n) :foo) n2 nil)) ]
    (is (= (meta-reduce2 n3 r)
            [n4 {:2 :1, :3 :3, :4, :4}])
      "One reduced and one not, in a vector.")))
      
(deftest node-to-vec
  (let [n1 (make-node :foo :1 {})
        n2 (make-node :quux :4 [ (make-node :bar :2 {}) ])
        n3 (make-node :baz :3 {
              :children
              n1
            })
        n4 (make-node :baz :3 {
              :children 
              n2
            })
        r (fn [n] (if (= (node-type n) :foo) n2 nil)) 
        ]
    (is (= (meta-reduce2 n3 r)
            [n4 {:3 :3, :4 :1}])
      "A node is reduced to a vector (and the original id is lost in the process for now).")))
      
(deftest node-to-vec2
  (let [n2 (make-node :quuux :5 [
                (make-node :bar :2 {})
              ])
        n5 (make-node :quux :4 {})
        n3 (make-node :baz :3 {
              :children 
              (make-node :foo :1 {})
            })
        n4 (make-node :baz :3 {
              :children 
              (make-node :quuux :5 [
                n5
                ])
            })
        r (fn [n] (condp = (node-type n)
                    :foo n2
                    :bar n5 
                    nil)) ]
    (is (= (meta-reduce2 n3 r)
            [n4 {:3 :3, :5 :1}])  ; TODO: what?
      "A node is reduced to a vector, and then an element of the vector is reduced.")))
      
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


(deftest ids-via-reduction
  (let [f (fn [n v] [nil (conj v (node-id n))])]
    (is (= (reduce-plus (node :foo :core/id :1) f #{})
            [(node :foo :core/id :1) {:1 :1} #{:1}]))))

(deftest apply-until1
  (let [n (fn [n] nil)
        i (fn [n] n)]
    (is (= ((apply-until) 1)
          nil)
        "none")
    (is (= ((apply-until n) 1)
          nil)
        "just nil")
    (is (= ((apply-until i) 1)
          1)
        "just id")
    (is (= ((apply-until n i) 1)
          1)
        "nil first")
    (is (= ((apply-until i n) 1)
          1)
        "id first")
    (is (= ((apply-until n n n n n n n n) 1)
          nil)
        "lotta nil")
    (is (= ((apply-until n n n n n n n n i) 1)
          1)
        "eventually id")))
        
(deftest with-attr1
  (is (= (with-attr (node :foo) :foo/bar bar (node :baz :quux bar) 1)
          1)
          "missing")
  (is (= (with-attr (node :foo :bar (make-node :bar :2 2)) :foo/bar bar (node :baz :core/id :1 :quux bar) 1)
          (node :baz :core/id :1 :quux (make-node :bar :2 2)))
          "present"))
