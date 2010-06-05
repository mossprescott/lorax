; A language for expressions in any programming language, with reduction to
; the low-level view language.

(ns meta.edit.expr
	(:use (meta core reduce)))
	
; Nodes:

; :view/expr/box

; Different kinds of infix expressions; each has an "items" attribute which is a sequence of boxes
; :view/expr/juxt - adjacent items with no extra space
; :view/expr/binary - adjacent with thinspace (expected to be expressions alternating with ordinary operators)
; :view/expr/relation - adjacent with mediumspace (expected to be expressions alternating with "relational" operators)
; :view/expr/flow - adjacent with quad

; Different kinds of characters, using different fonts, etc.; each has a str attr.
; :view/expr/keyword
; :view/expr/symbol - "str" may be a keyword naming a symbol (using the TeX name)
; :view/expr/var
; :view/expr/int

; :view/expr/scripted
; - /nucleus, /super?, /sup?


(defn- synthetic-color-node
  []
  (node :view/rgb :red 0.5 :green 0.5 :blue 1))

(defn parenthesize
  "Reduction that adds parens around nested expressions where spacing isn't
  sufficient to make the meaning clear, including:
  - binary or relation inside binary, relation, flow, or juxt
  - any compund expression which is the nucleus of a :scripted
  Note: this results in a mixed-language program, with both expr- and view-
  language nodes, since it introduces :view/parens nodes but does not reduce
  anything."
  [n]
  (let [ wrappableEmbeddings #{ 
          ; pairs of [ parent-type, child-type ] for which the child requires parens:
          [ :view/expr/juxt     :view/expr/juxt ]
          [ :view/expr/juxt     :view/expr/binary ]
          [ :view/expr/juxt     :view/expr/relation ]
          [ :view/expr/juxt     :view/expr/flow ]
          [ :view/expr/binary   :view/expr/binary ]
          [ :view/expr/binary   :view/expr/relation ]
          [ :view/expr/binary   :view/expr/flow ]
          [ :view/expr/relation :view/expr/binary ]
          [ :view/expr/relation :view/expr/relation ]
          [ :view/expr/relation :view/expr/flow ]
          [ :view/expr/flow     :view/expr/flow ]
        }
        parenNode 
          (fn [n]
            (node :view/parens
              :left "("
              :right ")"
              :view/drawable/color (synthetic-color-node)
              :content n))
        reduceBinary
          (fn [n]
              (let [;kw (keyword (subs (str (node-type n) "/boxes") 1))
                    ptype (node-type n)
                    wrappable (fn [n] (wrappableEmbeddings [ ptype (node-type n) ] ))
                    wrap (fn [n]
                          (if (wrappable n)
                            (parenNode n)
                            n))
                    boxes (node-attr n :boxes)]
                ; (println "kw:" kw)
                ; (println "n:") (print-node n)
                ; Note: need to make sure some child needs wrapping and otherwise return nil
                ; to avoid infinite loops:
                (if (and (vector? boxes) (some wrappable boxes))
                  (node (node-type n)
                    :boxes
                    (vec (map wrap boxes)))
                  nil)))
        reduceScripted
          (fn [n]
            (let [compound #{ :view/expr/juxt :view/expr/binary :view/expr/relation :view/expr/flow}
                  nucl (node-attr n :view/scripted/nucleus)
                  sup (node-attr n :view/scripted/super)]
              (if (contains? compound (node-type nucl))
                (node :view/scripted
                  :nucleus
                  (parenNode nucl)
                
                  :super
                  sup))))
        rules {
          :view/expr/juxt reduceBinary
          :view/expr/binary reduceBinary
          :view/expr/relation reduceBinary
          :view/expr/flow reduceBinary
          
          :view/scripted reduceScripted
        }]
    ; (print-node n)
    (first (meta-reduce2 n (reduceByType rules)))))


(def PRIME ["\u0030" :cmsy10])

(def SYMBOLS {
  "=" [ "=" :cmr10 ]
  
  ; "+" [ "\u002b" :times ]
  "+" [ "+" :cmr10 ]

  "-" [ "\u00c0" :cmsy10 ]  ; 0x00
   
  ; :times [ "\u00d7" :times ]
  ; !!! Should be x02, according to the table on the jsMath site, even, but 
  ; this works and matches what jsMath actually does
  :times [ "\u00c2" :cmsy10 ]
  
  "!" [ "!" :cmr10 ]
  
  :to [ "\u0021" :cmsy10 ]
  
  :lambda [ "\u00d5" :cmmi10 ]
  
  "?" [ "?" :cmr10 ]
  "|" [ "\u006a" :cmsy10 ]
  "*" [ "*" :cmr10 ]  ; cmsy has a centered asterix at 0x03
  
  :mapsto [ "\u0037\u0021" :cmsy10 ]  ; \mapstochar + \rightarrow
})


; Reduction to the lower-level language:
; TODO: keep track of [displaymode (D(D'),T,S,SS), meta-level]
; TODO: select font based on mode
; TODO: reduce core/later and core/sooner using meta-level
(defn- black-node [] (node :view/gray :brightness 0.0))

(def exprRules {
  :view/expr/juxt
  (fn [n]
    (node :view/sequence
      :items
      (node-attr n :view/expr/juxt/boxes)))

  :view/expr/binary
  (fn [n]
    (node :view/sequence
      :items
      (vec (interpose (node :view/thinspace) (node-attr n :view/expr/binary/boxes)))))
  
  :view/expr/relation
  (fn [n]
    (node :view/sequence
      :items
      (vec (interpose (node :view/mediumspace) (node-attr n :view/expr/relation/boxes)))))

  :view/expr/flow
  (fn [n]
    (node :view/sequence
      :items
      (vec (interpose (node :view/thickspace) (node-attr n :view/expr/flow/boxes)))))

  :view/expr/keyword
  (fn keyword [n]
    (node :view/chars
      :str (node-attr n :view/expr/keyword/str)
      :font :cmbx10))

  :view/expr/symbol
  (fn [n]
    (assert (contains? SYMBOLS (node-attr n :view/expr/symbol/str)))  ; not great, you really want to know the symbol that wasn't found...
    (let [ [c f] (SYMBOLS (node-attr n :view/expr/symbol/str)) ]
      (node :view/chars
        :str c
        :font f)))

  ; TODO: handle primes and subscripts somehow?
  :view/expr/var
  (fn [n]
    (node :view/chars
      :str (node-attr n :view/expr/var/str)
      :font :cmmi10))
      ; :font :timesItalic))  ;; HACK

  :view/expr/int
  (fn [n]
    (node :view/chars
      :str (node-attr n :view/expr/int/str)
      :font :cmr10))

  :view/expr/string
  (fn [n]
    (node :view/chars
      :str (str \u005c (node-attr n :view/expr/string/str) "\"")
      :font :cmr10
      :view/drawable/color (node :view/rgb :red 0 :green 0.5 :blue 0)))

  :view/expr/mono
  (fn [n]
    (node :view/chars
      :str (node-attr n :view/expr/mono/str)
      :font :courier))
  
  :view/expr/prod
  (fn [n]
    (node :view/chars
      :str (node-attr n :view/expr/prod/str)
      :font :courierItalic))
  
  :view/expr/missing
  (fn [n]
    (node :view/chars
      :str "?"
      :font :cmr10
      :view/drawable/color (synthetic-color-node)))

  ; TODO: use bg instead of border (requires an inherited attr.)
  :view/expr/later
  (fn [n]
    (node :view/border
      :weight 1
      :margin 2
      :view/drawable/colors [
        (node :view/gray :brightness 0.5)
        (node :view/gray :brightness 0.9)
      ]
      
      :item 
      (with-attr-node n :view/expr/later/node)))

  ; TODO: use bg instead of border (requires an inherited attr.)
  :view/expr/sooner
  (fn [n]
    (node :view/border
      :weight 1
      :margin 2
      :view/drawable/colors [
        (node :view/gray :brightness 0.9)
        (node :view/gray :brightness 0.5)
      ]
      
      :item 
      (with-attr-node n :view/expr/later/node)))


  ; HACK: this is easier than actually implementing growable parens for now, but this
  ; node should really be handled in nodes.clj with custom rendering
  :view/parens
  (fn [n]
    (node :view/sequence
      :items [
        (node :view/chars
          :str (node-attr n :left)
          :font :cmr10
          :view/drawable/color (with-attr n :view/drawable/color c c (black-node)))
        (n :view/parens/content)
        (node :view/chars
          :str (node-attr n :right)
          :font :cmr10
          :view/drawable/color (with-attr n :view/drawable/color c c (black-node)))
      ]))
})


;
; "Meta-reduction" for the view/expr language, used for rendering grammars, etc.
; This is a bit tricky in that the result of the reduction is new nodes in the 
; same grammar. A helper function takes of that.
;

; reduceOnce :: node -> (node -> Maybe node) -> (node -> Maybe node)
(defn reduceOnce
  "Given a node and reduction, return a new reduction which applies the given 
  function only to nodes of the original program (that is, it does _not_ 
  recursively reduce nodes that are the result of a reduction)."
  [n r]
  (let [ids (set (deep-node-ids n))
        _ (println "source ids:" ids)]
    (fn [np] 
      (if (ids (node-id np))
        (do (println "reduce!" (node-id np)) (r np))
        (do (println "no reduction" (node-id np)) nil)))))

(def metaExprRules-foo
  (letfn [ (borderize [b title]
              (fn [n]
                ; TODO: titled borders?
                (node :view/border
                  :weight 1
                  :margin 1
    
                  :view/drawable/colors [
                    (node :view/gray :brightness b)
                  ]
    
                  :item
                  (rename-nodes n)))) ]  ; Tricky! need to rename the node being embedded in the result, so it won't be recursively reduced
  {  
    :view/juxt
    (borderize 0.7 "juxt")
  
    :view/expr/binary
    (borderize 0.7 "binary")
  
    :view/expr/relation
    (borderize 0.7 "relation")

    :view/expr/flow
    (borderize 0.7 "flow")

    :view/expr/keyword
    (borderize 0.7 "kw")

    :view/expr/symbol
    (borderize 0.7 "sym")

    :view/expr/var
    (borderize 0.7 "var")

    :view/expr/int
    (borderize 0.7 "int")
      ; (node :view/chars
      ;   :str (node-attr n :view/expr/int/str)
      ;   :font :cmr10))

    :view/expr/string
    (borderize 0.7 "str")

    :view/expr/mono
    (borderize 0.7 "mono")
  
    :view/expr/prod
    (borderize 0.7 "prod")
  }))


;
; Reduction of :view/expr nodes. Each node is reduced only once, and only if 
; its id appears in the set which is provided as the reduction's aux. value.
;
(def metaExprRules
  (letfn [ (borderize [b title]
              (fn [n v]
                ; TODO: titled borders?
                (let [id (node-id n)]
                  (if (contains? v id)
                    (let [ np (node :view/scripted
                                :nucleus
                                (node :view/border
                                  :weight 1
                                  :margin 1
    
                                  :view/drawable/colors [
                                    (node :view/gray :brightness b)
                                  ]
    
                                  :item
                                  n)
                                  
                                :super
                                (node :view/chars :str title :font :tiny)) ]
                      [ np (disj v id) ])
                    [ nil v ]))))]
  {  
    :view/juxt
    (borderize 0.7 "juxt")
  
    :view/expr/binary
    (borderize 0.7 "binary")
  
    :view/expr/relation
    (borderize 0.7 "relation")

    :view/expr/flow
    (borderize 0.7 "flow")

    :view/expr/keyword
    (borderize 0.7 "kw")

    :view/expr/symbol
    (borderize 0.7 "sym")

    :view/expr/var
    (borderize 0.7 "var")

    :view/expr/int
    (borderize 0.7 "int")
      ; (node :view/chars
      ;   :str (node-attr n :view/expr/int/str)
      ;   :font :cmr10))

    :view/expr/string
    (borderize 0.7 "str")

    :view/expr/mono
    (borderize 0.7 "mono")
  
    :view/expr/prod
    (borderize 0.7 "prod")
  }))