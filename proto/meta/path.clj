; Operations on paths, which identify nodes by their location, relative to 
; an arbitrary node (often the root node of some program).

; A path is a cons-list of keywords and integers. The tail of the list is the
; first step on the path, always the name of an attribute of the "root" node.
; Each successive element is either an attribute name or an integer indexing 
; into a sequence attr.

(ns meta.path
	(:use (clojure test)
	      (meta core)))

(defn node-paths
  "Sequence of paths to all descendant nodes, relative to the given node, "
  ([n]
    (node-paths n nil))
  ([n prefix]
    (apply concat (list prefix)
      (for [a (node-attrs n)]
        (let [v (node-attr n a)
              p (cons a prefix)
              ;_ (println a ";" v ";" p)
              ]
          (cond
            (node? v)
            (node-paths v p)
          
            (vector? v)
            (assert false)
          
            true 
            [p]))))))

(defn node-at-path
  [n p]
  (if (empty? p)
    n
    (let [h (first p)
          t (rest p)]
      (node-at-path (n h) t))))  ; TODO: use node-attr/nth for node/vector (kw/int)

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


; (pr (node-paths (node :a :b 1)))
; (println)

(deftest node-paths1
  (is (= #{"" "a/b"}
          (set (map path-to-str (node-paths (node :a :b 1)))))))
  
(deftest node-at-path1
  (is (= (node :foo :core/id :1)
        (node-at-path (node :foo :core/id :1) '())))
  (is (= "a"
        (node-at-path 
          (node :foo :bar "a")
          '(:foo/bar)))))
  
(deftest path-to-str1
  (is (= "")
        (path-to-str '()))
  (is (= "a:b[1]:c"
        (path-to-str '(:c 1 :b :a)))))
