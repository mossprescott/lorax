(ns inspect
  (:use clojure.inspector clojure.xml))

;(use 'clojure.xml) 

(set! *warn-on-reflection* true)

(def foo (clojure.xml/parse "foo.xml"))

(println "foo.xml: " foo)