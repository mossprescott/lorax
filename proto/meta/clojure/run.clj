; Consumes a :clojure/core/program, expanding and then evaluating each expr
; and building a :clojure/core/session.

(ns meta.clojure.run
  (:use (meta core check reduce)
        (meta.clojure core kernel)))

(def kernel-grammar
  (load-node "meta/clojure/kernel2.mlj"))
  
(def kernel-structure
  (grammar-to-structure kernel-grammar))

(def kernel-checker
  (make-structure-checker kernel-structure))

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

(defn expansionsByType
  "Map from node types to functions which may be applied to a node of the 
  specified type to produce an expanded node, for each rule in the given
  grammar which has an :expand attribute.
  The reduction functions are built by constructing a :clojure/kernel/lambda
  node and then compiling it."
  [gr]
  (reduce merge {} 
    (for [r (node-attr gr :rules)]
      (if (has-attr? r :expand)
        (let [ t (node-attr r :type)
                as (attrs-by-id (node-attr r :display))
                ; _ (println as)
                exp (node-attr r :expand)
                
                ; HACK: assuming one child for now
                [cid cname] (first as)

                qf (node :clojure/kernel/lambda
                    :params [
                      (node :clojure/kernel/bind :core/id :n)                    
                    ]
                    
                    :body
                    (node :clojure/kernel/let
                      :bind
                      (node :clojure/kernel/bind :core/id cid)
                      
                      :expr
                      (node :clojure/kernel/app
                        :expr
                        (node :clojure/kernel/extern
                          :name "node-attr")
                          
                        :args [
                          (node :clojure/kernel/var
                            :ref (ref-node :n))
                            
                          (node :clojure/kernel/app
                            :expr
                            (node :clojure/kernel/extern
                              :name "keyword")
                            
                            :args [
                              (node :clojure/kernel/string
                                :value (subs (str cname) 1))  ;; TODO???
                            ])
                        ])
                        
                      :body
                      exp))
                  ; _ (println "qf:")
                  ; _ (print-node qf true)
                  ; _ (println (doall (kernel-checker qf)))
                  cf (meta-compile qf)
                  ; _ (println cf)
                  f (eval cf)
                  ;_ (println f)
                  ]
          { t 
            ; (fn [n] exp)
            f })  ; HACK
        {}))))

(def core-expansions
  (let [c (load-node "meta/clojure/core.mlj")]
    (expansionsByType c)))

; (println (keys core-expansions))

(defn expand
  [n exp]
  (if-let [f (exp (node-type n))]
    ; (do
    ;   (println "f:" f)
    ;   (println "expand:")
    ;   (print-node n true)
    ;   (println "to:")
    ;   (println (f n))
    (f n)
    ))
    ; rename the source nodes so they can be added to the result tree
    ;(rename-nodes n)))

(defn run-program
  [n exp]
  (node :clojure/core/session
    :exchanges
    (vec (for [x (node-attr n :exprs)]
            (let [ [xp o] (meta-reduce2 x #(expand % exp))]
              (node :clojure/core/exchange
                :expr
                x
                
                :kernel
                xp
                
                :value
                (meta-eval xp)))))))

; (def x1
;   (node :clojure/core/program
;     :exprs [
;       (node :clojure/kernel/nil)
;     ]))
;     
; (print-node (run-program x1 core-expansions))