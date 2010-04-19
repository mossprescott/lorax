; Support for "checker" functions, which run over the structure of a node
; and produce a map of node ids to vectors of errors (just strings, at the 
; moment).

(ns meta.check
  (:use (clojure set test)
        (meta core)))
  

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
  [options n value]
  (if (some #(checkOption % value) options)
    []
    [ [ (node-id (if (node? value) value n))
        (str "unexpected value: " (if (node? value) (node-type value) value)) ] ]))

(defn- checkSimpleAttr
  [attr n value]
  (if (vector? value)
    [ [ (node-id n) (str "expected a single value, found a sequence for attribute: " (node-attr attr :structure/simpleAttr/name)) ] ]
    (checkValue (node-attr attr :structure/simpleAttr/options) n value)))

(defn- checkSequenceAttr
  [attr n value]
  (if (not (vector? value))
    [ [ (node-id n) (str "expected a sequence, found a single value for attribute: " (node-attr attr :structure/sequenceAttr/name)) ] ]
    (apply concat 
      (for [v value] (checkValue (node-attr attr :structure/sequenceAttr/options) n v)))))

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
  Of course, it would probably be possible to compile it instead..."
  [struc]
  (let [ visitor (fn [n env] 
                    [ (checkNode struc n) nil ]) ]
    (fn [n]
      (reduce merge-seq {} 
        (for [ [k v] (apply concat (visitNode n visitor nil)) ]
          { k [v] })))))


(defn invert-set-map
  "Returns a map where each element of each val is mapped to a set of the keys that 
  contain it."
  [m]
  (reduce 
    #(merge-with union {} %1 %2)
    (apply concat
      (for [ [k s] m ] 
        (for [v s]
          {v #{k}})))))
          
(deftest invert-set-map1
  (is (= (invert-set-map { :a #{1 2} :b #{2 3} })
        {3 #{:b}, 2 #{:a :b}, 1 #{:a}})))


(defn- makeOptions
  [ts instances]
  (vec (apply concat 
    (for [t ts] 
      (if-let [iset (instances t)]
        (for [i iset]
          (node :structure/node :type i))
        [ (node :structure/node :type t) ])))))


(defn- findAttrs
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
                               (set (node-attr n :grammar/rule/supers)) }
                            {})
                          nil])
                      nil))
        instances (invert-set-map supers)
        _ (println instances)]
    (node :structure/language
      :rules
      (vec (for [r (node-attr grammar :grammar/language/rules)]
              (node :structure/rule
                :type                 
                (node-attr r :grammar/rule/type)
                
                :attrs
                (vec (findAttrs (node-attr r :grammar/rule/display) instances)))))))) ; TODO


(deftest grammar1
  (is (= 1 1))) ; TODO  

;
; Compile a grammar program to a "display" reduction:
;


(defn grammar-to-display
  "Takes a :grammar/language node and returns a map of type to reduction functions."
  [grammar]
  ()
  )



;
; Presentation for the structure specification language:
;

(defn- simpleName
  "For now, remove any prefix from every name, to make things easy to read.
  The right thing might be something more like stripping off prefixes that 
  are obvious only."
  [kw]
  (let [#^String s (subs (str kw) 1)
        idx (.lastIndexOf s (int \/))]  ; a clean way to do this in Clojure?
    (if (= idx -1)
        s
        (subs s (inc idx)))))
        
;
; Presentation reduction for the primitive structure language:
;

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
  
  :structure/string
  (fn [n]
    (node :view/expr/keyword :str "string"))
  
  :structure/node
  (fn [n]
    (node :view/expr/prod :str (simpleName (node-attr n :structure/node/type))))
  
  :structure/ref
  (fn [n]
    (node :view/expr/flow
      :boxes [
        (node :view/expr/keyword :str "ref")
        (node :view/expr/prod :str (simpleName (node-attr n :structure/ref/type)))
      ]))
  })

;
; Tests:
;

(deftest simple
  (let [f (fn [n env] [(node-type n) env])
        n1 (node :foo )
        n2 (node :bar :attr n1) 
        n3 (node :baz :attr [ n1 n2 ])]
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
