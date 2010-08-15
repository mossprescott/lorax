; Consumes a :clojure/core/program, expanding and then evaluating each expr
; and building a :clojure/core/session.

(ns meta.clojure.run
  (:use (meta core check reduce)
        (meta.clojure core kernel)))

(def kernel-grammar
  (load-node "meta/clojure/kernel.mlj"))
  
; (def kernel-structure
;   (grammar-to-structure kernel-grammar))

(def kernel-checker
  ; (make-structure-checker kernel-structure))
  (fn [n] {}))  ; HACK: no-op for now

(defn- attrs-by-id
  "Given a display node, finds all attribute nodes and returns a map from their
  ids to the names of the attributes they declare."
  [dn]
  (reduce merge {}
    (visitNode
      dn
      (fn [n env] 
        [ (condp = (node-type n)
            :grammar/sequence
            { (node-id n) (node-attr n :grammar/sequence/name) }
            
            :grammar/attr
            { (node-id n) (node-attr n :grammar/attr/name) }
            
            {})
          nil ])
      nil)))


; 
; (defn expansionsByType
;   "Map from node types to functions which may be applied to a node of the 
;   specified type to produce an expanded node, for each rule in the given
;   grammar which has an :expand attribute.
;   The reduction functions are built by constructing a :clojure/kernel/lambda
;   node and then compiling it."
;   [gr]
;   (reduce merge {} 
;     (for [r (node-attr gr :rules)]
;       (if (has-attr? r :expand)
;         (let [ t (node-attr r :type)
;                 as (attrs-by-id (node-attr r :display))
;                 ; _ (println as)
;                 exp (node-attr r :expand)  ; TODO: rename, to avoid id collisions!
;                 
;                 ; new id for the arg of the expansion fn:
;                 nid (genid "n")
; 
;                 bindChildren (fn bindChildren [idAndNamePairs]
;                                 (let [s (seq idAndNamePairs)]
;                                   (if s
;                                     (let [ [cid cname] (first s) ]
;                                       (node :clojure/kernel/let
;                                         :bind
;                                         (node :clojure/kernel/bind :core/id cid)
;                       
;                                         :expr
;                                         (node :clojure/kernel/app
;                                           :expr
;                                           (node :clojure/kernel/extern
;                                             :name "node-attr")
;                           
;                                           :args [
;                                             (node :clojure/kernel/var
;                                               :ref (ref-node nid))
;                             
;                                             (node :clojure/kernel/app
;                                               :expr
;                                               (node :clojure/kernel/extern
;                                                 :name "keyword")
;                             
;                                               :args [
;                                                 (node :clojure/kernel/string
;                                                   :value (subs (str cname) 1))  ;; TODO???
;                                               ])
;                                           ])
;                         
;                                         :body
;                                         (bindChildren (rest idAndNamePairs))))
;                                       exp)))
; 
;                 qf (node :clojure/kernel/lambda
;                     :params [
;                       (node :clojure/kernel/bind :core/id nid)
;                     ]
;                     
;                     :body
;                     (bindChildren as))
;                 
;                 ; _ (println "qf:")
;                 ; _ (print-node qf true)
;                 ; _ (println (doall (kernel-checker qf)))
;                 cf (meta-compile qf)
;                 ; _ (println cf)
;                 f (eval cf)
;                 ;_ (println f)
;                 ]
;           { t 
;             ; (fn [n] exp)
;             f })  ; HACK
;         {}))))

(def expand-core
  (let [c (load-node "meta/clojure/core.mlj")]
    (reduce-with-grammar c :expand)))

; (println (keys core-expansions))

; (defn expand
;   [n exp]
;   (if-let [f (exp (node-type n))]
;     ; (do
;     ;   (println "f:" f)
;     ;   (println "expand:")
;     ;   (print-node n true)
;     ;   (println "to:")
;     ;   (println (f n))
;     (f n)
;     ))
;     ; rename the source nodes so they can be added to the result tree
;     ;(rename-nodes n)))

(defn run-program
  [n exp]
  (make-node :clojure/core/session
    (vec (for [x (node-children n)]
            (let [ ; _ (println "x:" x)
                   [xp o] (try (meta-reduce2 x exp)
                               (catch Throwable x
                                      (println x)
                                      (make-node :clojure/kernel/nil))) ; HACK
                  
                  ; _ (println "xp:" xp)
                  ; _ (println "cxp:" (meta-compile x))
                  r (try (meta-eval xp) 
                         (catch Throwable x 
                                (println x)
                                (make-node :clojure/kernel/nil))) ; HACK
                  ; _ (println "r:" r)
                  ]
              (make-node :clojure/core/exchange {
                :expr
                x

                ; Comment to disable:
                :kernel
                xp
                
                :value
                r
              }))))))

; (def x1
;   (make-node :clojure/core/program [
;       (make-node :clojure/kernel/nil)
;     ]))
;     
; (print-node (run-program x1 expand-core))