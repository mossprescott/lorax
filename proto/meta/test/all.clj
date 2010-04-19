; Runs all the tests in the listed namespaces.

(ns meta.test.all
  (:use (clojure test))
  (:require (meta reduce check))
  (:require (meta.edit draw)))
  
(run-tests 'meta.core 'meta.reduce 'meta.check 'meta.edit.draw)
