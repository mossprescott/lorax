; Operations on paths, which identify nodes by their location, relative to 
; an arbitrary node (often the root node of some program).

; Current implementation:
; A path is a cons-list of keywords and integers. The tail of the list is the
; first step on the path, always the name of an attribute of the "root" node.
; Each successive element is either an attribute name or an integer indexing 
; into a sequence attr.

(ns meta.path
	(:use (clojure test)
	      (meta core)))

(defn node-paths
  "Sequence of paths to all descendant nodes, relative to the given node."
  ([n]
    (node-paths n nil))
  ([n prefix]
    (apply concat 
      [prefix]
      (for [a (node-attrs n)]
        (let [v (node-attr n a)
              p (cons a prefix)
              ;_ (println a ";" v ";" p)
              ]
          (cond
            (node? v)
            (node-paths v p)
          
            (vector? v)
            (apply concat 
              [p]
              (for [i (range (count v))]
                (let [c (nth v i)
                      pp (cons i p)]
                  (if (node? c)
                    (node-paths c pp)
                    [pp]))))
          
            true 
            [p]))))))

(defn node-at-path
  [n p]
  (if (empty? p)
    n
    (let [h (first p)
          t (rest p)
          np (node-at-path n t)]
      (cond 
        (node? np)
        (node-attr np h)
        
        (vector? np)
        (nth np h)
        
        true
        (assert false)))))

(defn path-to-str
  "Unpack a path into a string which reads left-to-right."
  [p]
  (if (empty? p)
    ""
    (let [h (first p)
          t (rest p)]
      (if (keyword? h)
        (let [s (subs (str h) 1)]
          (if (empty? t)
            s
            (str (path-to-str t) \: s)))
        (str (path-to-str t) \[ h \])))))

(defn parent-path
  "Given a path, returns the path to the parent node or sequence."
  [p]
  (rest p))

(defn child-paths
  [root p]
  (let [n (node-at-path root p)]
    (cond
      (node? n)
      (for [a (node-attrs n)]
        (cons a p))
        
      ; (vector? n)
      ; (for [i (range (count n))]
      ;   (cons i p))
      
      true
      ())))

(defn sibling-paths
  "Sequence of paths to all nodes which are children of the node's parent."
  [root p]
  (child-paths root (parent-path p)))

(defn root-path?
  [p]
  (empty? p))



; (pr (node-paths (node :a :b 1)))
; (println)

(deftest node-paths1
  (is (= #{"" "a/b"}
          (set (map path-to-str (node-paths (node :a :b 1))))))
  (is (= #{"" "a/b" "a/c" "a/c[0]" "a/c[1]" "a/c[1]:d/e"}
          (set (map path-to-str 
               (node-paths 
                 (node :a 
                   :b 1
                   :c [2 (node :d :e 3)])))))))
  
(deftest node-at-path1
  (is (= (node :foo :lorax/id :1)
        (node-at-path (node :foo :lorax/id :1) '())))
  (is (= "a"
        (node-at-path 
          (node :foo :bar "a")
          '(:bar))))
  (is (= "c"
        (node-at-path 
          (node :foo :bar ["a" "b" "c"])
          '(2 :bar)))))
  
(deftest path-to-str1
  (is (= "")
        (path-to-str '()))
  (is (= "a:b[1]:c"
        (path-to-str '(:c 1 :b :a)))))
