; Runs all the tests in the listed namespaces.

(ns meta.test.all
  (:use (clojure test))
  (:require (meta reduce check path))
  (:require (meta.edit draw)))
  
(run-tests 'meta.core 'meta.reduce 'meta.check 'meta.path 
  'meta.edit.draw
  'meta.clojure.kernel)
