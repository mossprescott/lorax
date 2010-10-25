; Support for "checker" functions, which run over the structure of a node
; and produce a map of node ids to vectors of errors (just strings, at the 
; moment). The main checker is an interpreter for the :structure language, 
; which is a low-level language for structural constraints.

; Also, compilers/interpreters for the higher-level :grammar language.

(ns meta.check
  (:use (clojure set test)
        (meta core reduce)
        (meta.clojure kernel)))

; Structure language:
; Expresses constraints on what types of nodes and values can inhabit the 
; attributes of each node type.
; This language will be used both by the checker fxn defined here, and by 
; the editor directly, to help constrain the choices presented in the UI.
;
; (see structure.mlj)

; (defn- getRule
;   "Rule node for the given type, or nil"
;   [struc nodeType]
;   (let [ matches (for [r (node-attr struc :structure/language/rules) 
;                     :when (= nodeType (node-attr r :structure/rule/type))] r) ]
;     (condp = (count matches)
;       0 nil
;       1 (first matches)
;       (assert false))))  ; multiple rules for this type; TODO: better error?
; 
; (defn- getAttrName
;   [attr]
;   (node-attr attr (keyword (subs (str (node-type attr) "/name") 1))))
; 
; (defn- getAttr
;   "SimpleAttr or sequenceAttr node for the given attribute name, or nil"
;   [rule attrName]
;   (let [matches (for [a (node-attr rule :structure/rule/attrs) 
;                     :when (= attrName (getAttrName a))] a) ]
;     (condp = (count matches)
;       0 nil
;       1 (first matches)
;       (assert false))))  ; multiple rules for this attr; TODO: better error?
; 
; (defn- checkOption
;   [option value]
;   (condp = (node-type option)
;     :structure/node (and (node? value)
;                       (= (node-type value) (node-attr option :structure/node/type)))
;     :structure/name (keyword? value)
;     true))  ; TODO: rest of the types
; 
; (defn- checkValue
;   [options n value label]
;   (if (some #(checkOption % value) options)
;     []
;     [ [ (node-id (if (node? value) value n))
;         (str "unexpected value for " label ": " (if (node? value) (node-type value) value)) ] ]))
; 
; (defn- checkSimpleAttr
;   [attr n value]
;   (if (vector? value)
;     [ [ (node-id n) (str "expected a single value, found a sequence for attribute: " (node-attr attr :structure/simpleAttr/name)) ] ]
;     (checkValue 
;       (node-attr attr :structure/simpleAttr/options) 
;       n 
;       value 
;       (node-attr attr :structure/simpleAttr/name))))
; 
; (defn- checkSequenceAttr
;   [attr n value]
;   (if (not (vector? value))
;     [ [ (node-id n) (str "expected a sequence, found a single value for attribute: " (node-attr attr :structure/sequenceAttr/name)) ] ]
;     (apply concat 
;       (for [i (range (count value))] (checkValue 
;                                         (node-attr attr :structure/sequenceAttr/options) 
;                                         n 
;                                         (get value i) 
;                                         (str (node-attr attr :structure/sequenceAttr/name) "[" i "]"))))))
; 
; (defn- checkAttr
;   [attr n value]
;   (condp = (node-type attr)
;     :structure/simpleAttr (checkSimpleAttr attr n value)
;     :structure/sequenceAttr (checkSequenceAttr attr n value)
;     (assert false)))
; 
; (defn- checkNode
;   "Given a structure and node, returns a sequence containing errors for the 
;   single node and its attributes: [ id errorMsg ]."
;   [struc n]
;   (let [nodeType (node-type n)
;         rule (getRule struc nodeType)]
;     (cond
;       (= nodeType :core/ref) []  ; don't check refs at all for now
;       (nil? rule) [ [(node-id n) (str "unrecognized node type: " nodeType)] ]
;       ; TODO: also look for missing required attributes
;       true (apply concat
;             (for [attrName (node-attrs n)]
;               (let [attr (getAttr rule attrName)]
;                 (if (nil? attr)
;                   [ [(node-id n) (str "unrecognized attribute: " attrName)] ]
;                   (checkAttr attr n (node-attr n attrName)))))))))
; 
; (defn- assoc-seq
;   "Assoc a new value with a key in map, building a vector of values for each key."
;   [map key vals]
;   (assoc map key (concat (get map key []) vals)))
; 
; (defn- merge-seq
;   [a b]
;   (reduce #(apply assoc-seq %1 %2) a b))
  

; (defn make-structure-checker
;   "From a set of rules which describe what parent-child relationships are allowed, 
;   builds a checker. The rules take the form of a program in the 'structure'
;   language, which gives a completely explicit declaration of what children are 
;   expected for each type of node.
;   As a checker, the resulting fxn takes a node and returns a map of node ids to
;   errors.
;   The function is therefore essentially an interpreter for the structure language
;   Of course, it would be possible to compile it instead..."
;   [struc]
;   (let [ visitor (fn [n env] 
;                     [ (checkNode struc n) nil ]) ]
;     (fn [n]
;       (reduce merge-seq {} 
;         (for [ [k v] (apply concat (visitNode n visitor nil)) ]
;           { k [v] })))))


;
; Compiler from the :grammar language to the :structure language:
;

; (defn invert-set-map
;   "Returns a map where each element of each val is mapped to a set of the keys that 
;   contain it."
;   [m]
;   (reduce 
;     #(merge-with union {} %1 %2)
;     {}
;     (apply concat
;       (for [ [k s] m ] 
;         (for [v s]
;           {v #{k}})))))
;           
; (deftest invert-set-map1
;   (is (= (invert-set-map { :a #{1 2} :b #{2 3} })
;         {3 #{:b}, 2 #{:a :b}, 1 #{:a}}))
;   (is (= (invert-set-map {})
;           {})))
; 
; 
; (defn- makeOptions
;   "Given a sequence of node/ref/int/string nodes and a map from type names to
;   names of specific instances, returns a sequence of :structure/node nodes."
;   [os instances]
;   (vec (apply concat 
;     (for [opt os]
;       (if (= :structure/node (node-type opt))
;         (let [typ (node-attr opt :structure/node/type)] 
;           (if-let [iset (instances typ)]
;             (for [i iset]
;               (node :structure/node :type i))
;             [ (node :structure/node :type typ) ]))
;         [ opt ])))))


; (defn findAttrs
;   "Given the node which is the :display attribute of some rule and a map of 
;   instances of each abstract node type, produces a sequence of :structure/attr
;   nodes defining all attributes present in the sub-tree."
;   [dn instances]
;   (filter #(not (nil? %))
;     (visitNode
;       dn
;       (fn [n env] 
;         [ (condp = (node-type n)
;             :grammar/sequence
;             (node :structure/sequenceAttr
;               :name 
;               (node-attr n :grammar/sequence/name)
;             
;               :options
;               (makeOptions (node-attr n :grammar/sequence/options) instances)
;             
;               :min 
;               (node-attr n :grammar/sequence/min))
;             
;             :grammar/attr
;             (node :structure/simpleAttr
;               :name
;               (node-attr n :grammar/attr/name)
;             
;               :options
;               (makeOptions (node-attr n :grammar/attr/options) instances)
;               
;               :optional false)
;             
;             nil)
;           nil ])
;       nil)))


; (deftest grammar1
;   (is (= 1 1))) ; TODO  

;
; Compile/interpret a grammar program to/as a "display" reduction:
;

(defn- bind-attrs
  [d attrs]
  (if-not (seq attrs)
    ; Tricky: need to rename the root node of the reduced tree, since it will 
    ; appear multiple times in the target tree. Only the root is renamed, 
    ; because the tree will contain other nodes that are part of the source 
    ; tree and we don't want to lose track of those.
    ; d
    (make-node :clojure/kernel/app {
              :expr
              (make-node :clojure/kernel/extern { :name "meta.core/rename-node" })  ; Note: needs qualification only on _second_ use!
              
              :args
              (make-node :clojure/kernel/args [
                d
              ])
            })
    (let [a (first attrs)]
      (make-node :clojure/kernel/let {
        :bind
        (make-node :clojure/kernel/bind (node-id a) {})
    
        :expr
        (make-node :clojure/kernel/app {
          :expr
          (make-node :clojure/kernel/extern { :name "meta.reduce/with-attr-node" })  ; Note: needs qualification only on _second_ use!
        
          :args 
          (make-node :clojure/kernel/args [
            (make-node :clojure/kernel/var { :ref (ref-node :node) })
            (make-node :clojure/kernel/name { :value (node-attr-value a :name) })
          ])
        })
    
        :body
        (bind-attrs d (rest attrs))
      }))))

(defn- display-fn
  [mn attr]
  (let [red (node-attr mn attr)]  ; TODO: reduce the reduction using what exactly?
    (make-node :clojure/kernel/lambda {
      :params 
      (make-node :clojure/kernel/params [
        (make-node :clojure/kernel/bind :node {})
      ])
      
      :body
      (bind-attrs red (node-attr-children mn :attrs))
    })))


(defn- reduce-seq
  "Reduction function for the :display nodes of seqNode rules. Replaces any 
  :grammar/seq node(s) with an expanded sequence.
  This isn't a nice general-purpose evaluation like we do for map-nodes, but it
  works."
  [elems]
  (fn [n]
    (if (= (node-type n) :grammar/seq)
      (let [val (if-not (has-attr? n :separator)
                  elems
                  (interpose (node-attr n :separator) elems))  ; TODO: rename the separator?
            s (make-node (node-attr-value n :type)
                         val)]
          s))))


(defn reduce-with-rule
  "Given a rule node and reduction attr name, returns a reduction function
  or nil."
  [rule attr]
  (if (has-attr? rule attr)
    (try 
      (condp = (node-type rule)
        :grammar/mapNode
        (let [_ (println (format "compiling reduction for %s (%s)..." (node-attr-value rule :type) attr))  ; HACK
              ; _ (println "found rule:")  ; HACK
              ; _ (print-node rule true)  ; HACK
              mdf (display-fn rule attr)
              ; _ (println "mdf:")  ; HACK
              ; _ (print-node mdf true)  ; HACK
              cl (meta-compile mdf)
              ; _ (when (= :clojure/core/where (node-attr-value rule :type)) ; HACK
              ;     (print-node mdf true) ; HACK
              ;     (println "cl:" cl))  ; HACK
              ; _ (println "cl:" cl)  ; HACK
              df (eval cl)
              ; _ (println "df:" df)  ; HACK
              ; np (df n)
              ; _ (print-node np true)  ; HACK
              ]
          df)
    
        :grammar/seqNode
        (let [red (node-attr rule attr)]
          (fn [n]
            (let [redr (rename-nodes red)
                  f (reduce-seq (node-children n))
                  ; _ (println "seq:" (node-type n))
                  ]
              ; (let [r ; HACK
              (rename-node   ; rename the root node, which will be replicated in the target tree
                (meta-reduce redr f))
              ; ] (print-node r true) r) ; HACK
                )))
      
        true
        (assert false))
      (catch Exception x
        (println (format "Error while constructing reduction for %s (%s): %s"
                          (node-attr rule :type)
                          attr
                          x))
        nil))))


(defn reduce-with-grammar
  "Takes a :grammar/language node and returns a reduction function which
  performs one of the reductions described in the grammar (i.e. :display or 
  :expand).
  In any case, :expand reductions are compiled and used to reduce each 
  subsequent declaration, so that any synta intorduced in a rule is valid in
  any subsequent rule."
  [grammar attr]
  ; TODO: reduce each reduction with reductions compiled so far!
  ; (let [fnsByType (mapfor [rule (node-children grammar)]
  ;                         (node-attr-value rule :type)
  ;                         (reduce-with-rule rule attr))
  (let [fnsByType (loop [rules (node-children grammar)
                         fns {}
                         expfns {}]
                    (if-let [rule (first rules)]
                      (let [rrule (meta-reduce rule (reduceByType expfns))
                            typ (node-attr-value rrule :type)
                            ; _ (if (= typ :clojure/core/in) (print-node rrule)) ; HACK
                            f (reduce-with-rule rrule attr)
                            e (reduce-with-rule rrule :expand)]
                        (recur (next rules) 
                               (assoc fns typ f)
                               (assoc expfns typ e)))
                      fns))
        ; _ (doseq [[k v] fnsByType]  ; HACK
        ;       (println k v))
        f (reduceByType fnsByType)]
    f))
    ; (fn [n] 
    ;   (if-let [np (f n)]
    ;     (rename-nodes np)))))  ; rename _after_ reduction, since reduced nodes can appear multiple times
  

(defn grammar-to-display
  "Takes a :grammar/language node and returns a reduction function which
  performs the presentation reduction described in the grammar."
  [grammar]
  (reduce-with-grammar grammar :display))


(defn compose-grammars
  ; TODO: this should be a trivial operation on nodes -- make a new node with 
  ; the concatenated children of some nodes. What language is provided for that?
  [& more]
  (make-node :grammar/language
    (vec (mapcat node-children more))))

;
; Presentation for the structure specification language:
;

(defn- baseName
  "Remove any prefix from every name, to make things easy to read (but possibly 
  ambiguous)."
  [kw]
  (let [#^String s (subs (str kw) 1)
        idx (.lastIndexOf s (int \/))]  ; a clean way to do this in Clojure?
    (if (= idx -1)
        s
        (subs s (inc idx)))))

(defn- simpleName
  "For now, remove any prefix from every name, to make things easy to read.
  The right thing might be something more like stripping off prefixes that 
  are obvious only.
  [[Actually, now I'm leaving the prefixes in, for clarity, and just removing
  the colon that makes it a keyword for Clojure...]]"
  ([kw]
    (subs (str kw) 1))
  ([parent kw]
    (simpleName kw)))
    ; (let [pstr (str parent)
    ;       kstr (str kw)]
    ;   (if (.startsWith kstr pstr)
    ;     (subs kstr (count pstr))
    ;     (subs kstr 1)))))
    

;
; Presentation for the higher-level :grammar language. This is a temporary 
; measure to get things working; eventually the grammar language should be 
; self-describing.
;

(def grammarPresRules {
  :grammar/language
  (fn [n] 
    (make-node :view/section
      (interpose (make-node :view/expr/keyword { :str " " })
                 (node-children n))))
  
  :grammar/mapNode
  (fn [n]
    (make-node :view/section [
        (with-attr n :doc e
          e
          (make-node :view/sequence [])) ; HACK: empty node
        (make-node :view/expr/flow [
            (node-attr n :supers)
            (make-node :view/expr/symbol { :str :from })
            (node-attr n :type)
            (node-attr n :attrs)
          ])
        (make-node :view/sequence [
          (make-node :view/quad)
          (with-attr n :display e
            e
            (make-node :view/sequence [])) ; HACK: empty node
          (with-attr n :expand e
            (make-node :view/sequence [
                (make-node :view/thickspace)
                (make-node :view/expr/symbol { :str :to })
                (make-node :view/thickspace)
                e
              ])
            (make-node :view/sequence [])) ; HACK: empty node
        ])
      ]))
      
  :grammar/seqNode
  (fn [n]
    (make-node :view/section [
        (with-attr n :doc e
          e
          (make-node :view/sequence [])) ; HACK: empty node
        (make-node :view/expr/flow [
            (node-attr n :supers)
            (make-node :view/expr/symbol { :str :from })
            (node-attr n :type)
            (make-node :view/delimited {
              :left "{"
              :right "}"
              :content
              (make-node :view/scripted {
                  :nucleus
                  (node-attr n :options)
                
                  :super
                  (make-node :view/expr/juxt [
                    (make-node :view/expr/int { :str (str (node-attr-value n :min)) })
                    (make-node :view/expr/keyword { :str ".." })
                    ; TODO: max?
                  ])
                })
              })
            ])

        (make-node :view/sequence [
            (make-node :view/quad)
            (node-attr n :display)
            
            (with-attr n :expand e
              (make-node :view/sequence [
                  (make-node :view/thickspace)
                  (make-node :view/expr/symbol { :str :to })
                  (make-node :view/thickspace)
                  e
                ])
              (make-node :view/sequence [])) ; HACK: empty node
          ])
      ]))

  :grammar/seq
  (fn [n]
    (make-node :view/expr/embed {
      :content
      (make-node (node-attr-value n :type) [
        (make-node :view/expr/var { :str "elem" })
        (with-attr n :separator s
          s
          (make-node :view/sequence []))
        (make-node :view/chars { :str "..." :font :cmr10 })
      ])
    }))
      
  :grammar/doc
  (fn [n] 
    (make-node :view/expr/doc { :str (node-attr n :str) }))
  
  :grammar/types
  (fn [n]
    (make-node :view/expr/juxt
      (vec (interpose
              (make-node :view/sequence [
                  (make-node :view/expr/keyword { :str "," })
                  (make-node :view/thinspace {})
                ])
              (node-children n)))))

  :grammar/type
  (fn [n]
    (make-node :view/expr/prod {
        :str (baseName (node-value n))
      }))
  
  :grammar/star  ; a special "type" which may appear anywhere
  (fn [n]
    (make-node :view/expr/symbol { :str "*" }))
  
  :grammar/attrs
  (fn [n]
    (make-node :view/delimited {
      :left "{"
      :right "}"
      :content
      (make-node :view/sequence
        (vec (interpose
                (make-node :view/sequence [
                    (make-node :view/expr/keyword { :str "," })
                    (make-node :view/thickspace {})
                  ])
                (node-children n))))
    }))
    ; (make-node :view/section
    ;   (node-children n)))

  :grammar/attr
  (fn [n]
    (make-node :view/expr/binary [
        (node-attr n :name)
        (make-node :view/expr/keyword { :str ":" })
        (node-attr n :options)
        (if (and (has-attr? n :optional) (node-attr-value n :optional))
          (make-node :view/expr/symbol { :str "?" })
          (make-node :view/sequence []))  ; HACK: empty
      ]))
  
  :grammar/name
  (fn [n]
    (make-node :view/expr/var {
        :str (baseName (node-value n))
      }))
      
  :grammar/options
  (fn [n] 
    (make-node :view/expr/relation
      (interpose (make-node :view/expr/symbol { :str "|" })
                 (node-children n))))
  
  :grammar/ref
  (fn [n]
    (make-node :view/expr/binary [
      (make-node :view/expr/keyword { :str "ref" })
      (make-node :view/expr/prod { :str (baseName (node-attr-value n :type)) })
    ]))

  ; :grammar/ref
  ; (fn [n]
  ;   (make-node :view/expr/unbed {
  ;       :content
  ;       (node-attr n :ref)
  ;       ; (make-node :view/expr/var { :str (baseName (ref-node-id (node-attr n :ref))) })  ; HACK: will be handled by the name reduction
  ;     }))
  
  :grammar/node
  (fn [n]
    (make-node :view/expr/prod { 
        :str (baseName (node-attr-value n :type))
      }))  ; TODO
  
  :grammar/int
  (fn [n]
    (make-node :view/expr/keyword { :str "int" }))

  :grammar/string
  (fn [n]
    (make-node :view/expr/keyword { :str "string" }))

  :grammar/nameValue  ; Note: to avoid conflicting with grammar/name
  (fn [n]
    (make-node :view/expr/keyword { :str "name" }))

  ; HACK? used in seqNode/min
  :grammar/count
  (fn [n]
    (make-node :view/expr/int { :str (str (node-value n)) }))

  })

;
; Tests:
;

(deftest simple
  (let [f (fn [n env] [(node-type n) env])
        n1 (make-node :foo)
        n2 (make-node :bar { :attr n1 }) 
        n3 (make-node :baz [ n1 n2 ])]
    (is (= (visitNode n1 f nil)
            [:foo]))
    (is (= (set (visitNode n2 f nil))
            #{:foo :bar}))
    (is (= (set (visitNode n3 f nil))
            #{:foo :bar :baz}))))
            
; (deftest assoc-seq1
;   (is (= (assoc-seq {} :a [:b])
;         { :a [:b] })))
; 
; (deftest merge-seq1
;   (is (= (merge-seq {:a [1] :b [2]} {:a [3] :c [4]})
;         {:a [1 3] :b [2] :c [4]})))
