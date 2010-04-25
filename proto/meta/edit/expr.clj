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
              (let [kw (keyword (subs (str (node-type n) "/boxes") 1))
                    ptype (node-type n)
                    wrappable (fn [n] (wrappableEmbeddings [ ptype (node-type n) ] ))
                    wrap (fn [n]
                          (if (wrappable n)
                            (parenNode n)
                            n))
                    boxes (n kw)]
                ; (println "kw:" kw)
                ; (println "n:") (print-node n)
                ; Note: need to make sure some child needs wrapping and otherwise return nil
                ; to avoid infinite loops:
                (if (some wrappable boxes)
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
(def exprRules {
  :view/expr/juxt
  (fn relation [n]
    (node :view/sequence
      :items
      (node-attr n :view/expr/juxt/boxes)))

  :view/expr/binary
  (fn binary [n]
    (node :view/sequence
      :items
      (vec (interpose (node :view/thinspace) (node-attr n :view/expr/binary/boxes)))))
  
  :view/expr/relation
  (fn relation [n]
    (node :view/sequence
      :items
      (vec (interpose (node :view/mediumspace) (node-attr n :view/expr/relation/boxes)))))

  :view/expr/flow
  (fn relation [n]
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

  ; HACK: this is easier than actually implementing growable parens for now, but this
  ; node should really be handled in nodes.clj with custom rendering
  :view/parens
  (fn [n]
    (node :view/sequence
      :items [
        (node :view/chars
          :str (node-attr n :view/parens/left)
          :font :cmr10
          :view/drawable/color (node-attr n :view/drawable/color))
        (n :view/parens/content)
        (node :view/chars
          :str (node-attr n :view/parens/right)
          :font :cmr10
          :view/drawable/color (node-attr n :view/drawable/color))
      ]))
})