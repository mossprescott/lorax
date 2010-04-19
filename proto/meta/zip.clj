; Not-really-working functional zipper attempt...

(ns meta.zip
  ; (:require clojure.zip :as zip))
  (:require ['clojure.zip :as 'z]))
  
(defn node-zip
  "Make a zipper on the given node. The generic zipper API might not actually 
  work, though: my children are named, and unordered.
  For the experiment, a node is one of:
  - a node, representing the root node or a node in a vector (sequence attr.)
  - a list '(attrName value), representing an attribute, where the value might
    be a node, string, keyword, int, or vector of values
  Problem: inserting a node with a name that exists?"
  [n]
  (zip/zipper 
    (fn branch? 
      [n] 
      (if (vec? x)
        (node? (second x))
        true))
    (fn children 
      [x] 
      (let [n (if (vec? x) (second x) x) ]
        (for [a (node-attrs n)] [a (node-attr n a)])))  ; TODO: sort?
    (fn make-node 
      [x children]
      nil)
      ; (if (vec? n)
      ;   (let [ [a n] ))  ; TODO
    n))

