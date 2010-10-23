; Operations for 'editing' trees, in the form of a set of primitives which
; transform a given tree into a new tree with some node(s) added or removed.
; 
; Since nodes are immutable, all operations work by constructing a new, 
; modified tree, which shares portions of its structure with the old tree.
;
; When necessary, nodes are "renamed" to preserve the uniqueness of ids.
;
; The node(s) within the tree to be modified are identified by id.

(ns meta.edit
  (:use
    (clojure test) 
    (meta core reduce)))

; 

; Would it make more sense to use paths? As in delete the node at this path, 
; insert this node at this path? It's not hard to do one in terms of the other,
; but the implementations would be different.

(defn delete-node
  "Construct an updated tree with a single node removed. If the node was
  part of a sequence, the length of the sequence is decreased by one. If the 
  node was part of a map-node, the attribute is simply removed. If the id 
  refers to the root node, or does not appear in the tree, an error occurs."
  [root id]
  (cond
    (= id (node-id root))
    (throw (AssertionError. "Cannot delete the root node"))
    
    (not (contains? (set (deep-node-ids root))
                    id))
    (throw (AssertionError. "Id not found to delete"))
    
    true
    (letfn [(f [n]
              (let [child-ids (set (map node-id (node-children n)))]
                (if (contains? child-ids id)
                  (cond 
                    (map-node? n)
                    (make-node (node-type n)
                               (node-id n)
                               (mapfor [a (node-attrs n) :when (not= id (node-id (node-attr n a)))]
                                       a (node-attr n a)))
              
                    (seq-node? n)
                    (make-node (node-type n)
                               (node-id n)
                               (for [c (node-children n) :when (not= id (node-id c))] c))))))]
      (meta-reduce root f))))


(defn insert-node
  "Construct an updated tree with a single node inserted under a certain
  attribute name.
  If the parent is a map-node, the attr must be a name (keyword), and the 
  node must currently contain no such attribute.
  If the parent is a seq-node, then attr must be an index between 0 and the 
  number of nodes currently present, and the new node is inserted in that 
  position.
  [TODO: Before the node is inserted, any sub-nodes whose ids appear in the original
  tree are first re-named, following the usual approach of renaming only bound 
  ids.]"
  [root id attr newNode]
  (cond
    (not (contains? (set (deep-node-ids root))
                    id))
    (throw (AssertionError. "Id not found for insert"))
    
    true
    (letfn [(f [n done]
              (if (and (not done) (= id (node-id n)))
                [ (make-node (node-type n)
                            (node-id n)
                    (cond
                      (map-node? n)
                      (if (has-attr? n attr)
                        (throw (AssertionError. "Attribute already present"))
                        (assoc (mapfor [a (node-attrs n)] a (node-attr n a))
                            attr newNode))
                  
                      (seq-node? n)
                      (let [cs (node-children n)]
                        (if (or (< attr 0) (> attr (count cs)))
                          (throw (AssertionError. "Index out of bounds"))
                          (concat (take attr cs) [newNode] (drop attr cs))))
                  
                      true
                      (throw (AssertionError. "Not a map/seq node."))))
                  true ]))]
      (first (reduce-plus root f false)))))

(defn- parentIdAndAttr
  ; find the node by id and return a vector [ parentId attr ]
  [root id] 
  (first (filter #(not (nil? %))
    (for [n (visitNode root)]
      (first (for [a (node-attrs n) :when (= id (node-id (node-attr n a)))] 
                [(node-id n) a]))))))
                
(defn swap-nodes
  "Change the positions of two nodes appearing anywhere in the tree, given their ids."
  [root id1 id2]
  (let [n1 (find-node root id1)
        n2 (find-node root id2)
        [parentId1 attr1] (parentIdAndAttr root id1)
        [parentId2 attr2] (parentIdAndAttr root id2)]
    (-> root (delete-node id1)
              (delete-node id2)
              (insert-node parentId2 attr2 n1)
              (insert-node parentId1 attr1 n2))))

(defn replace-node
  "Remove an existing node and add a new one in its place."
  [root oldNodeId newNode]
  (let [ [parentId attr] (parentIdAndAttr root oldNodeId)]
    (-> root (delete-node oldNodeId)
            (insert-node parentId attr newNode))))

; Tests
(deftest delete1
  (is (thrown? AssertionError
        (delete-node (make-node :foo :123 {}) :123))
        "Can't delete the root node.")
  (is (thrown? AssertionError
        (delete-node (make-node :foo :123 {}) :456))
        "Node not present."))

(deftest delete-map1
  (is (= (make-node :foo {})
        (delete-node (make-node :foo { :bar (make-node :baz :1 {}) })
                    :1)))
  (is (= (make-node :foo { :baz 2 })
        (delete-node (make-node :foo { :baz 2 :bar (make-node :zeb :1 {}) })
                    :1))))

(deftest delete-seq1
  (is (= (make-node :foo [])
        (delete-node (make-node :foo [ (make-node :bar :1 {}) ])
                    :1)))
  (is (= (make-node :foo [ 2 ])
        (delete-node (make-node :foo [ 2 (make-node :zeb :1 {}) ])
                    :1)))
  (is (= (make-node :foo [ 1 3 ])
        (delete-node (make-node :foo [ 1 (make-node :zeb :2 {}) 3 ])
                    :2))))

(deftest insert1
  (is (thrown? AssertionError
        (insert-node (make-node :foo :123 {}) :456 :bar (make-node :baz)))
        "Node not present.")
  (is (thrown? AssertionError
        (insert-node (make-node :foo :123 { :bar 1 }) :123 :bar (make-node :baz)))
        "Attribute already present.")
  (is (thrown? AssertionError
        (insert-node (make-node :foo :123 []) :123 -1 (make-node :baz)))
        "Negative index.")
  (is (thrown? AssertionError
        (insert-node (make-node :foo :123 []) :123 1 (make-node :baz)))
        "Index too large."))

(deftest insert-map1
  (is (= (make-node :foo { :bar (make-node :baz) })
        (insert-node (make-node :foo :123 {}) :123 :bar (make-node :baz))))
  (is (= (make-node :foo { :bar 1 :baz 2 })
        (insert-node (make-node :foo :123 { :bar 1 }) :123 :baz (make-node :core/int 2)))))

(deftest insert-seq1
  (is (= (make-node :foo [ (make-node :bar) ])
        (insert-node (make-node :foo :123 []) :123 0 (make-node :bar))))
  (is (= (make-node :foo [ (make-node :baz) (make-node :bar) ])
        (insert-node (make-node :foo :123 [ (make-node :bar)]) :123 0 (make-node :baz))))
  (is (= (make-node :foo [ (make-node :bar) (make-node :baz) ])
        (insert-node (make-node :foo :123 [ (make-node :bar)]) :123 1 (make-node :baz)))))
