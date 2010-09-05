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

(defn- getRule
  "Rule node for the given type, or nil"
  [struc nodeType]
  (let [ matches (for [r (node-attr struc :structure/language/rules) 
                    :when (= nodeType (node-attr r :structure/rule/type))] r) ]
    (condp = (count matches)
      0 nil
      1 (first matches)
      (assert false))))  ; multiple rules for this type; TODO: better error?

(defn- getAttrName
  [attr]
  (node-attr attr (keyword (subs (str (node-type attr) "/name") 1))))

(defn- getAttr
  "SimpleAttr or sequenceAttr node for the given attribute name, or nil"
  [rule attrName]
  (let [matches (for [a (node-attr rule :structure/rule/attrs) 
                    :when (= attrName (getAttrName a))] a) ]
    (condp = (count matches)
      0 nil
      1 (first matches)
      (assert false))))  ; multiple rules for this attr; TODO: better error?

(defn- checkOption
  [option value]
  (condp = (node-type option)
    :structure/node (and (node? value)
                      (= (node-type value) (node-attr option :structure/node/type)))
    :structure/name (keyword? value)
    true))  ; TODO: rest of the types

(defn- checkValue
  [options n value label]
  (if (some #(checkOption % value) options)
    []
    [ [ (node-id (if (node? value) value n))
        (str "unexpected value for " label ": " (if (node? value) (node-type value) value)) ] ]))

(defn- checkSimpleAttr
  [attr n value]
  (if (vector? value)
    [ [ (node-id n) (str "expected a single value, found a sequence for attribute: " (node-attr attr :structure/simpleAttr/name)) ] ]
    (checkValue 
      (node-attr attr :structure/simpleAttr/options) 
      n 
      value 
      (node-attr attr :structure/simpleAttr/name))))

(defn- checkSequenceAttr
  [attr n value]
  (if (not (vector? value))
    [ [ (node-id n) (str "expected a sequence, found a single value for attribute: " (node-attr attr :structure/sequenceAttr/name)) ] ]
    (apply concat 
      (for [i (range (count value))] (checkValue 
                                        (node-attr attr :structure/sequenceAttr/options) 
                                        n 
                                        (get value i) 
                                        (str (node-attr attr :structure/sequenceAttr/name) "[" i "]"))))))

(defn- checkAttr
  [attr n value]
  (condp = (node-type attr)
    :structure/simpleAttr (checkSimpleAttr attr n value)
    :structure/sequenceAttr (checkSequenceAttr attr n value)
    (assert false)))

(defn- checkNode
  "Given a structure and node, returns a sequence containing errors for the 
  single node and its attributes: [ id errorMsg ]."
  [struc n]
  (let [nodeType (node-type n)
        rule (getRule struc nodeType)]
    (cond
      (= nodeType :core/ref) []  ; don't check refs at all for now
      (nil? rule) [ [(node-id n) (str "unrecognized node type: " nodeType)] ]
      ; TODO: also look for missing required attributes
      true (apply concat
            (for [attrName (node-attrs n)]
              (let [attr (getAttr rule attrName)]
                (if (nil? attr)
                  [ [(node-id n) (str "unrecognized attribute: " attrName)] ]
                  (checkAttr attr n (node-attr n attrName)))))))))

(defn- assoc-seq
  "Assoc a new value with a key in map, building a vector of values for each key."
  [map key vals]
  (assoc map key (concat (get map key []) vals)))

(defn- merge-seq
  [a b]
  (reduce #(apply assoc-seq %1 %2) a b))
  

(defn make-structure-checker
  "From a set of rules which describe what parent-child relationships are allowed, 
  builds a checker. The rules take the form of a program in the 'structure'
  language, which gives a completely explicit declaration of what children are 
  expected for each type of node.
  As a checker, the resulting fxn takes a node and returns a map of node ids to
  errors.
  The function is therefore essentially an interpreter for the structure language
  Of course, it would be possible to compile it instead..."
  [struc]
  (let [ visitor (fn [n env] 
                    [ (checkNode struc n) nil ]) ]
    (fn [n]
      (reduce merge-seq {} 
        (for [ [k v] (apply concat (visitNode n visitor nil)) ]
          { k [v] })))))


;
; Compiler from the :grammar language to the :structure language:
;

(defn invert-set-map
  "Returns a map where each element of each val is mapped to a set of the keys that 
  contain it."
  [m]
  (reduce 
    #(merge-with union {} %1 %2)
    {}
    (apply concat
      (for [ [k s] m ] 
        (for [v s]
          {v #{k}})))))
          
(deftest invert-set-map1
  (is (= (invert-set-map { :a #{1 2} :b #{2 3} })
        {3 #{:b}, 2 #{:a :b}, 1 #{:a}}))
  (is (= (invert-set-map {})
          {})))


(defn- makeOptions
  "Given a sequence of node/ref/int/string nodes and a map from type names to
  names of specific instances, returns a sequence of :structure/node nodes."
  [os instances]
  (vec (apply concat 
    (for [opt os]
      (if (= :structure/node (node-type opt))
        (let [typ (node-attr opt :structure/node/type)] 
          (if-let [iset (instances typ)]
            (for [i iset]
              (node :structure/node :type i))
            [ (node :structure/node :type typ) ]))
        [ opt ])))))


(defn findAttrs
  "Given a the node which is the :display attribute of some rule and a map of 
  instances of each abstract node type, produces a sequence of :structure/attr
  nodes defining all attributes present in the sub-tree."
  [dn instances]
  (filter #(not (nil? %))
    (visitNode
      dn
      (fn [n env] 
        [ (condp = (node-type n)
            :grammar/sequence
            (node :structure/sequenceAttr
              :name 
              (node-attr n :grammar/sequence/name)
            
              :options
              (makeOptions (node-attr n :grammar/sequence/options) instances)
            
              :min 
              (node-attr n :grammar/sequence/min))
            
            :grammar/attr
            (node :structure/simpleAttr
              :name
              (node-attr n :grammar/attr/name)
            
              :options
              (makeOptions (node-attr n :grammar/attr/options) instances)
              
              :optional false)
            
            nil)
          nil ])
      nil)))

(defn grammar-to-structure
  "Takes a :grammar/language node and returns a :structure/language node."
  [grammar]
  (let [supers (reduce (partial merge-with union {}) 
                    (visitNode 
                      grammar
                      (fn [n env] 
                        [ (if (= (node-type n) :grammar/rule)
                            { (node-attr n :grammar/rule/type)
                               (set (cons (node-attr n :grammar/rule/type) 
                                        (node-attr n :grammar/rule/supers))) }
                            {})
                          nil])
                      nil))
        instances (invert-set-map supers)
        ;_ (println instances)
        ]
    (make-node :structure/language
      (vec (for [r (node-attr grammar :grammar/language/rules)]
              (node :structure/rule
                :type                 
                (node-attr r :grammar/rule/type)
                
                :attrs
                (vec (findAttrs (node-attr r :grammar/rule/display) instances)))))))) ; TODO


; (deftest grammar1
;   (is (= 1 1))) ; TODO  

;
; Compile/interpret a grammar program to/as a "display" reduction:
;

(defn- getGrammarRule
  "Rule node for the given type, or nil"
  [grammar nodeType]
  (let [ matches (for [r (node-children grammar) 
                    :when (= nodeType (node-attr-value r :type))] r) ]
    (condp = (count matches)
      0 nil
      1 (first matches)
      (assert false))))  ; multiple rules for this type; TODO: better error?

; (defn- reduceEmbedded
;   [target]
;   (fn [n] 
;     ; Note: at this point, we are reducing a portion of the display AST for 
;     ; some node. _target_ is the source node, and _n_ is the node being reduced,
;     ; which came from somewhere inside the display subtree.
; 
;     ; (println "reduceEmbedded:")
;     ; (println "  " (node-type target))
;     ; (print-node n true)
;     
;     (condp = (node-type n)
;       :grammar/attr
;       ; (let [v (with-attr-node target (node-attr n :grammar/attr/name))]
;       ;     (str v))  ; HACK: automatically cast to string for now...
;       (with-attr-node target (node-attr n :grammar/attr/name) v
;           (if (not (node? v))
;             (str v)   ; HACK: automatically cast to string for now...
;             v))
;     
;       :grammar/sequence
;       (with-attr-seq target (node-attr n :grammar/sequence/name) s
;         (with-attr n :grammar/sequence/separator sep 
;           (vec (interpose sep s))
;           s))
;     
;       ; TODO: additional grammar variable types for int, string, name, boolean, 
;       ; etc., with appropriate display options for each. Also, sequences of any
;       ; of them.
;       
;       nil)))

(defn- bind-attrs
  [d attrs]
  (if-not (seq attrs)
    d
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
  ; (let [red (node-attr mn attr)]  ; Note: rename the reduction's nodes, because they may appear mult. times in the reduced program.
  (let [red (node-attr mn attr)
        ; _ (println "red:") _ (print-node red true)
        redp (rename-nodes red)  ; Note: rename the reduction's nodes, because they may appear mult. times in the reduced program.
        ; _ (println "redp:") _ (print-node redp true)
        ]
    (make-node :clojure/kernel/lambda {
      :params 
      (make-node :clojure/kernel/params [
        (make-node :clojure/kernel/bind :node {})
      ])
      
      :body
      (bind-attrs redp (node-attr-children mn :attrs))
    })))

(defn- reduce-seq
  "Reduction function for the :display nodes of seqNode rules. Replaces any 
  :grammar/seq node(s) with an expanded sequence.
  This isn't a nice general-purpose evaluation like we do for map-nodes, but it
  works."
  [elems]
  (reduceByType {
    :grammar/seq
    (fn [n]
      (let [val (if-not (has-attr? n :separator)
                  elems
                  (interpose (node-attr n :separator) elems))]
        (make-node (node-attr-value n :type)
                   val)))
  }))

(defn reduce-with-grammar
  "Takes a :grammar/language node and returns a reduction function which
  performs one of the reductions described in the grammar.
  Note that currently the reduction visits the grammar node each time it is 
  invoked (that is, for each source node), and then compiles a reduction on the
  spot -- it would be equally easy to pre-compile a reduction for each rule,
  and more efficient."
  [grammar attr]
  (fn [n]
    ; (println "type for display:" (node-type n))
    ; (print-node n true)
    (let [typ (node-type n)]
      (if-let [rule (getGrammarRule grammar typ)]
        (if (has-attr? rule attr)
          (condp = (node-type rule)
            :grammar/mapNode
            (let [; _ (println "found rule:")  ; HACK
                  ; _ (print-node rule true)  ; HACK
                  mdf (display-fn rule attr)
                  ; _ (println "mdf:")  ; HACK
                  ; _ (print-node mdf true)  ; HACK
                  cl (meta-compile mdf)
                  ; _ (when (= :clojure/core/square (node-attr-value rule :type)) 
                  ;     (print-node mdf true)
                  ;     (println "cl:" cl))  ; HACK
                  ; _ (println "cl:" cl)  ; HACK
                  df (eval cl)
                  ; _ (println "df:" df)  ; HACK
                  np (df n)
                  ; _ (print-node np true)  ; HACK
                  ]
              np)
          
            :grammar/seqNode
            ; nil
            ; (first (meta-reduce2 n (reduce-seq rule)))
            (meta-reduce (node-attr rule attr) (reduce-seq (node-children n)))
            
            true
            (assert false)))))))

(defn grammar-to-display
  "Takes a :grammar/language node and returns a reduction function which
  performs the presentation reduction described in the grammar."
  [grammar]
  (reduce-with-grammar grammar :display))

; (defn grammar-to-display1
;   "Takes a :grammar/language node and returns a reduction function which
;   performs a single step of the presentation reduction described in the 
;   grammar."
;   [grammar]
;   (fn [n]
;     ; (println "type for display:" (node-type n))
;     ; (print-node n true)
;     (let [typ (node-type n)]
;       (if-let [rule (getGrammarRule grammar typ)]
;         (let [;_ (println "found rule:")
;               ;_ (print-node rule true)
;               display (node-attr rule :grammar/rule/display)
;               displayp (rename-nodes display)
;               [np o] (meta-reduce2 displayp (reduceEmbedded n))]
;           np)
;         nil))))



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
    
        
(def structurePresRules {
  :structure/language
  (fn [n]
    (node :view/section
      :items 
      (node-attr n :structure/language/rules)))
  
  :structure/rule
  (fn [n]
    (node :view/section
      :items [
        (node :view/expr/prod :str (simpleName (node-attr n :structure/rule/type)))
        (node :view/sequence
          :items [
            (node :view/quad)
            (node :view/section
              :items
              (node-attr n :structure/rule/attrs))
          ])
      ]))
      
  :structure/simpleAttr
  (fn [n] 
    (node :view/expr/flow
      :boxes [
        (node :view/expr/mono :str (simpleName (node-attr n :structure/simpleAttr/name)))  ; TODO: remove the prefix?
        ; (node :view/chars :str "\u0037\u0021" :font :cmsy10)  ; HACK?
        (node :view/expr/symbol :str :mapsto)
        (let [options (node :view/expr/binary
                        :boxes
                        (vec (interpose 
                              (node :view/expr/symbol :str "|")
                              (node-attr n :structure/simpleAttr/options))))]
          (if (node-attr n :structure/simpleAttr/optional)
            (node :view/expr/juxt
              :boxes [
                options
                (node :view/expr/symbol :str "?")
              ])
            options))
      ]))
      
  :structure/sequenceAttr
  (fn [n] 
    (node :view/expr/flow
      :boxes [
        (node :view/expr/mono :str (simpleName (node-attr n :structure/sequenceAttr/name)))  ; TODO: remove the prefix?
        (node :view/expr/symbol :str :mapsto)
        ; (node :view/expr/juxt
        ;   :boxes [
        (node :view/scripted
          :nucleus
          (node :view/expr/binary  ; Note: duplicated from above, nearly
            :boxes
            (vec (interpose 
                (node :view/expr/symbol :str "|")
                (node-attr n :structure/sequenceAttr/options))))
          :super
          (node :view/expr/juxt
            :boxes [
              (node :view/chars 
                :str (str (node-attr n :structure/sequenceAttr/min))
                :font :cmr10-script)
              (node :view/chars
                :str ".."
                :font :cmr10-script)
              (node :view/chars 
                :str (if (contains? (node-attrs n) :structure/sequenceAttr/max) 
                        (str (node-attr n :structure/sequenceAttr/max))
                        "n")
                :font :cmr10-script)
            ]))
          ; (node :view/expr/symbol :str "*")  ; TODO: display min/max as superscript "0..n", e.g.
            ; ])
      ]))
      
  :structure/name
  (fn [n]
    (node :view/expr/keyword :str "name"))

  :structure/boolean
  (fn [n]
    (node :view/expr/keyword :str "boolean"))
  
  :structure/int
  (fn [n]
    (node :view/expr/keyword :str "int"))
  
  :structure/float
  (fn [n]
    (node :view/expr/keyword :str "float"))
  
  :structure/string
  (fn [n]
    (node :view/expr/keyword :str "string"))
  
  :structure/node
  (fn [n]
    (node :view/expr/prod :str (baseName (node-attr-value n :structure/node/type))))  ; TODO
  
  :structure/ref
  (fn [n]
    (node :view/expr/flow
      :boxes [
        (node :view/expr/keyword :str "ref")
        (node :view/expr/prod :str (baseName (node-attr-value n :structure/ref/type)))  ; TODO
      ]))

  :structure/any
  (fn [n]
    (node :view/expr/symbol :str "*"))
    
  })


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
        (make-node :view/expr/flow [
            (node-attr n :type)
            ; (make-node :view/expr/prod { 
            ;     :str (simpleName (node-attr-value n :type)) 
            ;   })
            (make-node :view/expr/symbol {
                :str :to
              })
            (node-attr n :supers)
          ])
        (make-node :view/sequence [
            (node :view/quad)
            (node-attr n :attrs)
          ])
        (make-node :view/sequence [
            (node :view/quad)
            (node-attr n :display)
          ])
        (with-attr n :expand e
          (make-node :view/sequence [
              (make-node :view/quad)
              (make-node :view/expr/symbol { :str :to })
              (make-node :view/thickspace)
              e
            ])
          (make-node :view/sequence [])) ; HACK: empty node
      ]))
      
  :grammar/seqNode
  (fn [n]
    (make-node :view/section [
        (make-node :view/expr/flow [
            (node-attr n :type)
            (make-node :view/expr/symbol {
                :str :to
              })
            (node-attr n :supers)
          ])
        (make-node :view/sequence [
            (make-node :view/quad)
            (make-node :view/scripted {
                :nucleus
                (node-attr n :options)
                
                :super
                (make-node :view/expr/juxt [
                  (node-attr n :min)
                  (make-node :view/expr/keyword { :str ".." })
                  ; TODO: max?
                ])
              })
          ])
        (make-node :view/sequence [
            (make-node :view/quad)
            (node-attr n :display)
          ])
      ]))

  :grammar/seq
  (fn [n]
    (make-node :view/expr/embed {
      :content
      (make-node (node-attr-value n :type) [
        (make-node :view/expr/var { :str "elem" })
        (node-attr n :separator)
        (make-node :view/chars { :str "..." :font :cmr10 })
      ])
    }))
      
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
    (make-node :view/section
      (node-children n)))

  :grammar/attr
  (fn [n]
    (make-node :view/expr/relation [
        (node-attr n :name)
        (make-node :view/expr/keyword { :str ":" })
        (node-attr n :options)
      ]))
  
  :grammar/name
  (fn [n]
    (make-node :view/expr/var {
        :str (baseName (node-value n))
      }))
      
  :grammar/options
  (fn [n] 
    (make-node :view/expr/binary
      (interpose (make-node :view/expr/symbol { :str "|" })
                 (node-children n))))
  
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

  :grammar/nameValue
  (fn [n]
    (make-node :view/expr/keyword { :str "name" }))

  ; HACK? used in seqNode/min
  :grammar/count
  (fn [n]
    (make-node :view/expr/int { :str (str (node-value n)) }))

  ; ; Tricky: a single sequence node lives where a vector of nodes is expected,
  ; ; so it has to be reduced to a vector of some kind or all hell breaks loose
  ; :grammar/sequence
  ; (fn [n]
  ;   ; (do (println n)
  ;   (let [name (baseName (node-attr n :name))  ; TODO: know the parent rule type, so it can be stripped?
  ;         v (node :view/expr/unbed
  ;             :content
  ;             (make-node :view/expr/relation [
  ;                 (node :view/expr/mono :str name)
  ;                 (node :view/expr/keyword :str ":")
  ;                 (node :view/scripted
  ;                   :nucleus
  ;                   (node :view/expr/relation 
  ;                     (vec (interpose
  ;                           (node :view/expr/symbol :str "|")
  ;                           (with-attr-seq n :options))))
  ;                   
  ;                   :super
  ;                   (node :view/chars :str "*" :font :cmr10-script))  ; HACK
  ;               ]))
  ;         e (node :view/expr/keyword :str "...")
  ;         cs [ v e ] ]
  ;     (if (has-attr? n :separator)
  ;       (vec (interpose (node-attr n :separator) cs))
  ;       cs)))
        
    ; :grammar/attr
    ; (fn [n]
    ;   (let [name (baseName (node-attr n :name))  ; TODO: know the parent rule type, so it can be stripped?
    ;         options (with-attr-seq n :options)
    ;         optNode (make-node :view/expr/binary 
    ;                   (vec (interpose
    ;                         (make-node :view/expr/symbol { :str "|" })
    ;                         (node-children options))))
    ;         onp (if (node-attr n :optional)
    ;               (make-node :view/expr/juxt [
    ;                   optNode
    ;                   (make-node :view/expr/keyword { :str "?" })
    ;                 ])
    ;                 optNode)
    ;         a (make-node :view/expr/relation [
    ;               (make-node :view/expr/mono { :str name })
    ;               (make-node :view/expr/keyword { :str ":" })
    ;               onp
    ;               ])
    ;         b (make-node :view/expr/unbed {
    ;               :content a
    ;             })] 
    ;         ; (node :view/border
    ;         ;                 :weight 1
    ;         ;                 :margin 1
    ;         ;                 :view/drawable/colors [ (node :view/rgb :red 0.9 :green 0.7 :blue 0.7) ]
    ;         ;                 :item a)]
    ;     b))
    
    ; HACK: _very_ temporary! until I get grammar-based reduction working again!
    :clojure/kernel/quote
    (fn [n]
      (make-node :view/expr/embed {
          :content
          (node-attr n :body)
        }))
    :clojure/kernel/unquote
    (fn [n]
      (make-node :view/expr/unbed {
          :content
          (node-attr n :body)
        }))
    :clojure/kernel/var
    (fn [n]
      (node-attr n :ref))
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
            
(deftest assoc-seq1
  (is (= (assoc-seq {} :a [:b])
        { :a [:b] })))

(deftest merge-seq1
  (is (= (merge-seq {:a [1] :b [2]} {:a [3] :c [4]})
        {:a [1 3] :b [2] :c [4]})))

; (deftest kernel
;   (let [k (make-structure-checker (first (load-nodes "meta/kernel.mlj")))]
;     (is (= (k (node :clojure/kernel/int :value 0))
;           []))))
