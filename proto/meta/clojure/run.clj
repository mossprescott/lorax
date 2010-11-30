; Consumes a :clojure/core/program, expanding and then evaluating each expr
; and building a :clojure/core/session.

(ns meta.clojure.run
  (:use (meta core check reduce)
        (meta.clojure kernel)))

(def kernel-grammar
  (compose-grammars
    (load-node "meta/clojure/kernel1.mlj")
    (load-node "meta/clojure/kernel2.mlj")))
    
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

(def core-grammar-sources [
  ; Note: core-ast is first (even before kernel), so it can be used in kernel
  ; presentation reductions.
  "meta/clojure/core-ast.mlj"
  "meta/clojure/kernel1.mlj" 
  "meta/clojure/kernel2.mlj" 
  "meta/example/tex/binaryNode.mlj" ; HACK
  "meta/clojure/core.mlj"
  "meta/example/tex/cons.mlj"  ; HACK
  "meta/clojure/core-seq.mlj"
  "meta/example/tex/and.mlj"  ; HACK
  "meta/example/tex/continued-grammar.mlj"  ; HACK
  "meta/example/tex/continued-ops.mlj"  ; HACK
])

(def expand-core
  (let [c (apply compose-grammars (map load-node core-grammar-sources))]
            ; (load-node "meta/clojure/core.mlj")
            ; (load-node "meta/clojure/core-seq.mlj")
            ; (load-node "meta/example/tex/and.mlj") ; HACK: for demo purposes
            ; (load-node "meta/example/tex/continued-grammar.mlj") ; HACK: for demo purposes
            ; (load-node "meta/example/tex/continued-ops.mlj") ; HACK: for demo purposes
            ; )]
    (println "Compiling core reductions...")
    ; (let [r (reduce-with-grammar c :expand)]
    ;       (fn [n] (println (node-type n)) (r n)))))
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
  [n exp show-reduced]
  (make-node :clojure/core/session
    (vec (for [x (node-children n)]
            (if (#{ :clojure/core/doc :clojure/core/comment } (node-type x))
              x
              (let [ ; _ (println "x:") _ (print-node x true) ; HACK
                     [xp o] (try (meta-reduce2 x exp)
                                 (catch Throwable x
                                        (println x)
                                        (make-node :clojure/kernel/nil))) ; HACK
                  
                    ; _ (println "xp:") _ (print-node xp true) ; HACK
                    ; _ (println "cxp:" (meta-compile xp)) ; HACK
                    r (try (unread (meta-eval xp))
                           (catch Throwable x 
                                  (println x)
                                  (make-node :clojure/kernel/nil))) ; HACK
                    ; _ (println "r:" r)
                    ]
                (if show-reduced
                  (make-node :clojure/core/exchange {
                    :expr   x
                    :kernel (rename-nodes xp)
                    :value  r
                  })
                  (make-node :clojure/core/exchange {
                    :expr   x
                    :value  r
                  }))))))))

; (def x1
;   (make-node :clojure/core/program [
;       (make-node :clojure/kernel/nil)
;     ]))
;     
; (print-node (run-program x1 expand-core))