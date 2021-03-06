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
; :view/expr/flow - adjacent with quad (for expressions alternating with keywords, mostly)

; Different kinds of characters, using different fonts, etc.; each has a str attr.
; :view/expr/keyword
; :view/expr/symbol - "str" may be a keyword naming a symbol (using the TeX name)
; :view/expr/var
; :view/expr/int

; :view/expr/scripted
; - /nucleus, /super, /sub?


(defn- synthetic-color-node
  []
  (node :view/rgb :red 0.5 :green 0.5 :blue 1))

(defn parenthesize
  "Reduction that adds parens around nested expressions where spacing isn't
  sufficient to make the meaning clear, including:
  - binary or relation inside binary, relation, flow, or juxt
  - any compound expression which is the nucleus of a :scripted
  Note: this results in a mixed-language program, with both expr- and view-
  language nodes, since it introduces :view/delimited nodes but does not reduce
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
          
          ; [ :view/expr/relation :view/expr/binary ]
          [ :view/expr/relation :view/expr/relation ]
          [ :view/expr/relation :view/expr/flow ]
          
          [ :view/expr/flow     :view/expr/flow ]
          
          ; TODO: radical with super/sub?
        }
        parenNode 
          (fn [n]
            (make-node :view/delimited {
                :left "("
                :right ")"
                :view/drawable/color (synthetic-color-node)
                :content n
              }))
        reduceSeq
          (fn [n]
              (let [ptype (node-type n)
                    wrappable (fn [c] (and
                                        (node? c)
                                        (contains? wrappableEmbeddings [ ptype (node-type c) ] )
                                        (< 1 (count (node-children c)))))
                    wrap (fn [n]
                          (if (wrappable n)
                            (parenNode n)
                            n))
                    boxes (node-children n)]
                ; Note: need to make sure some child needs wrapping and otherwise return nil
                ; to avoid infinite loops:
                (if (some wrappable boxes)
                  (make-node ptype
                             (vec (map wrap boxes)))
                  nil)))
        reduceScripted
          (fn [n]
            (let [compound #{ :view/expr/juxt :view/expr/binary :view/expr/relation :view/expr/flow}
                  nucl (node-attr n :view/scripted/nucleus)
                  sup (node-attr n :view/scripted/super)]
              (if (and (node? nucl)
                        (contains? compound (node-type nucl))
                        (< 1 (count (node-children nucl))))
                (make-node :view/scripted {
                    :nucleus
                    (parenNode nucl)
                
                    :super
                    sup
                  }))))
        rules {
          :view/expr/juxt reduceSeq
          :view/expr/binary reduceSeq
          :view/expr/relation reduceSeq
          :view/expr/flow reduceSeq
          
          :view/scripted reduceScripted
        }]
    ; (print-node n)
    (meta-reduce2 n (reduceByType rules))))


(def SYMBOLS {
  "=" [ "=" :cmr10 ]
  
  ; "+" [ "\u002b" :times ]
  "+" [ "+" :cmr10 ]

  ; :times [ "\u00d7" :times ]
  ; !!! Should be x02, according to the table on the jsMath site, even, but 
  ; this works and matches what jsMath actually does
  
  "!" [ "!" :cmr10 ]
  
  :from [ "\u00ef" :cmsy10 ]  ; aka leftarrow; should be 0x20, according the the table
  :to [ "\u0021" :cmsy10 ]  ; aka rightarrow

  ; long arrows, built from a '-' and arrow char run together
  :longleftarrow [ "\u00ef\u00c0" :cmsy10 ]  ; TODO: need a -3mu kern between the chars (how?)
  :longrightarrow [ "\u00c0\u0021" :cmsy10 ]
  
  :alpha  [ "\u00cb" :cmmi10 ]
  :beta   [ "\u00cc" :cmmi10 ]
  :delta  [ "\u00ce" :cmmi10 ]
  :epsilon [ "\u00cf" :cmmi10 ]
  :theta  [ "\u00d2" :cmmi10 ]
  :lambda [ "\u00d5" :cmmi10 ]
  :mu     [ "\u00d6" :cmmi10 ]
  :pi     [ "\u00d9" :cmmi10 ]
  :rho    [ "\u00da" :cmmi10 ]
  :omega  [ "\u00db" :cmmi10 ]
  :sigma  [ "\u00dc" :cmmi10 ]
  :tau    [ "\u00b6" :cmmi10 ]
  :phi    [ "!" :cmmi10 ]
  :star   [ "?" :cmmi10 ]
  
  "?" [ "?" :cmr10 ]
  "|" [ "\u006a" :cmsy10 ]
  "||" [ "\u006b" :cmsy10 ]
  "*" [ "*" :cmr10 ]
  
  :mapsto [ "\u0037\u0021" :cmsy10 ]  ; \mapstochar + \rightarrow
  
  ">" [ ">" :cmmi10 ]
  "<" [ "<" :cmmi10 ]
  
  "." [ "." :cmr10 ]
  
  :leftarrow    [ "\u00ef" :cmsy10 ]  ; see :from
  :rightarrow   [ "\u0021" :cmsy10 ]
  :uparrow      [ "\u0022" :cmsy10 ]
  :downarrow    [ "\u0023" :cmsy10 ]
  :leftrightarrow   [ "\u0024" :cmsy10 ]
  :nearrow      [ "\u0025" :cmsy10 ]
  :searrow      [ "\u0026" :cmsy10 ]
  :simeq        [ "\u0027" :cmsy10 ]
  :Leftarrow    [ "\u0028" :cmsy10 ]
  :Rightarrow   [ "\u0029" :cmsy10 ]
  :Uparrow      [ "\u002a" :cmsy10 ]
  :Downarrow    [ "\u002b" :cmsy10 ]
  :Leftrightarrow [ "\u002c" :cmsy10 ]
  :nwarrow      [ "\u002d" :cmsy10 ]
  :swarrow      [ "\u002e" :cmsy10 ]
  :propto       [ "\u002f" :cmsy10 ]
  
  :prime   [ "\u0030" :cmsy10 ]
  :infty   [ "\u0031" :cmsy10 ]
  :in      [ "\u0032" :cmsy10 ]
  :ni      [ "\u0033" :cmsy10 ]
  :not     [ "\u0036" :cmsy10 ]
  :forall  [ "\u0038" :cmsy10 ]
  :exists  [ "\u0039" :cmsy10 ]
  :neg     [ "\u003a" :cmsy10 ]
  :emptyset [ "\u003b" :cmsy10 ]
  :top     [ "\u003e" :cmsy10 ]
  :bot     [ "\u003f" :cmsy10 ]
  
  :cup     [ "\u005b" :cmsy10 ]
  :cap     [ "\u005c" :cmsy10 ]
  :uplus   [ "\u005d" :cmsy10 ]
  :vee     [ "\u005e" :cmsy10 ]
  :wedge   [ "\u005f" :cmsy10 ]
  :vdash   [ "\u0060" :cmsy10 ]
  :dashv   [ "\u0061" :cmsy10 ]

  :gg      [ "\u00b5" :cmsy10 ]
  
  "-"      [ "\u00c0" :cmsy10 ]  ; 0x00   
  :cdot    [ "\u00c1" :cmsy10 ]
  :times   [ "\u00c2" :cmsy10 ]
  :ast     [ "\u00c3" :cmsy10 ]
  :div     [ "\u00c4" :cmsy10 ]
  :diamond [ "\u00c5" :cmsy10 ]
  :pm      [ "\u00c6" :cmsy10 ]
  :mp      [ "\u00c7" :cmsy10 ]
  :oplus   [ "\u00c8" :cmsy10 ]
  :ominus  [ "\u00c9" :cmsy10 ]
  :otimes  [ "\u00ca" :cmsy10 ]
  :oslash  [ "\u00cb" :cmsy10 ]
  :odot    [ "\u00cc" :cmsy10 ]
  :bigcirc [ "\u00cd" :cmsy10 ]
  :circ    [ "\u00ce" :cmsy10 ]
  :bullet  [ "\u00cf" :cmsy10 ]

  :equiv  [ "\u00d1" :cmsy10 ]
  :sim    [ "\u00d8" :cmsy10 ]
  :approx [ "\u00d9" :cmsy10 ]
  :ll     [ "\u00dc" :cmsy10 ]

  ; :Sigma ??
  ; :Pi ??
  
  :sum [ "\u0050" :cmex10 ]
  :displaysum [ "\u0058" :cmex10 ]
  :prod [ "\u0051" :cmex10 ]
  :displayprod [ "\u0059" :cmex10 ]
  :bigwedge [ "\u0056" :cmex10 ]
  :displaybigwedge [ "\u005e" :cmex10 ]
  :bigvee [ "\u0057" :cmex10 ]
  :displaybigvee [ "\u005f" :cmex10 ]
  
  ; TODO: the rest of the symbols from tables on p. 435-437
})

;
; Reduction to the lower-level language:
;

(defn- black-node [] (node :view/gray :brightness 0.0))

; The value is a vector containing:
; - the display mode: one of #{ :D :d :T :t :S :s :SS :ss }, where the 
;   lowercase version of each is Knuth's "cramped", so my :d is his D'
; - the meta-level, an integer beginning at 0, incremented each time an :embed
;   node is encountered, and decremented on :unbed 

; Map/fn from mode to mode for superscripts:
(def superscript-mode {
  :D :S, :d :s,
  :T :S, :t :s,
  :S :SS, :s :ss,
  :SS :SS, :ss :ss
})

; Map/fn from mode to mode for subscripts:
(def subscript-mode {
  :D :s, :d :s,
  :T :s, :t :s,
  :S :ss, :s :ss,
  :SS :ss, :ss :ss
})

; Map/fn from mode to mode for numerators/denominators:
(def fraction-mode {
  :D :T, :d :t,
  :T :S, :t :s,
  :S :SS, :s :ss,
  :SS :SS, :ss :ss
})

; TODO: support "cramped" modes
; Map/fn from mode to cramped mode in same size:
; (def cramped {
;   :D :d, :d :d, 
;   :T :t, :t :t,
;   :S :s, :s :s,
;   :SS :ss, :ss :ss
; })

(def FONTS_BY_STYLE_AND_MODE {
  :keyword { :T :cmbx10, :S :cmbx10-script, :SS :cmbx10-scriptscript }
  :name { :T :cmbx10-it, :S :cmbx10-it-script, :SS :cmbx10-it-scriptscript }
  :symbol { :T :cmsy10, :S :cmsy10-script, :SS :cmsy10-scriptscript }
  ; :var { :T :cmmi10, :S :cmmi10-script, :SS :cmmi10-scriptscript }
  :var { :T :cmti10, :S :cmti10-script, :SS :cmti10-scriptscript }  ; use text italics, which have sensible spacing
  :int { :T :cmr10, :S :cmr10-script, :SS :cmr10-scriptscript }
  :string { :T :sans, :S :sans-script, :SS :sans-scriptscript }
})

(def EMBED_COLORS (cycle [
  (node :view/rgb :red 1.0 :green 1.0 :blue 1.0)  ; white
  (node :view/rgb :red 0.9 :green 0.9 :blue 1.0)  ; blue
  (node :view/rgb :red 0.9 :green 1.0 :blue 0.9)  ; green
  (node :view/rgb :red 0.8 :green 1.0 :blue 1.0)  ; cyan
  (node :view/rgb :red 1.0 :green 1.0 :blue 0.8)  ; yellow
  (node :view/rgb :red 1.0 :green 0.8 :blue 1.0)  ; magenta
  ]))

(def EMBED_BORDER_COLORS (cycle [
  (node :view/rgb :red 1.0 :green 1.0 :blue 1.0)  ; white
  (node :view/rgb :red 0.5 :green 0.5 :blue 0.8)  ; blue
  (node :view/rgb :red 0.5 :green 0.8 :blue 0.5)  ; green
  (node :view/rgb :red 0.3 :green 0.6 :blue 0.6)  ; cyan
  (node :view/rgb :red 0.6 :green 0.6 :blue 0.3)  ; yellow
  (node :view/rgb :red 0.6 :green 0.3 :blue 0.6)  ; magenta
  ]))


(defn- as-string
  [n]
  (if (and (value-node? n) (string? (node-value n)))
      (node-value n)
      (make-node :core/string (str (node-type n)))))

(defn exprToView 
  [n]
  (let [rules {
          ;
          ; Sequences:
          ;
          
          :view/expr/juxt
          (fn [n [mode level]]
            [ (make-node :view/sequence
                (node-children n))
              [mode level] ])

          :view/expr/binary
          (fn [n [mode level]]
            [ (make-node :view/sequence
                (vec (interpose (node :view/thinspace) 
                                (node-children n))))
              [mode level] ])
  
          :view/expr/relation
          (fn [n [mode level]]
            [ (make-node :view/sequence
                (vec (interpose (node :view/mediumspace) 
                                (node-children n))))
              [mode level] ])

          :view/expr/flow
          (fn [n [mode level]]
            [ (make-node :view/sequence
                (vec (interpose (node :view/thickspace) 
                                (node-children n))))
              [mode level] ])

          ;
          ; Atoms:
          ;

          :view/expr/keyword
          (fn [n [mode level]]
            [ (make-node :view/chars {
                :str (as-string (node-attr n :str))
                :font (-> FONTS_BY_STYLE_AND_MODE :keyword mode)
              })
              [mode level] ])

          :view/expr/name
          (fn [n [mode level]]
            [ (make-node :view/chars {
                :str (as-string (node-attr n :str))
                :font (-> FONTS_BY_STYLE_AND_MODE :name mode)
              })
              [mode level] ])

          :view/expr/symbol
          (fn [n [mode level]]
            (let [sname (node-attr-value n :str)]
              (assert-pred #(contains? SYMBOLS %) sname)  ; TODO: don't assert? instead reduce to what?
              (let [ [c f] (SYMBOLS sname)
                     fp (condp = f          ; HACK!
                          :cmsy10
                          (-> FONTS_BY_STYLE_AND_MODE :symbol mode)
                          
                          :cmr10
                          (-> FONTS_BY_STYLE_AND_MODE :int mode)
                          
                          f)
                    ]
                [ (make-node :view/chars {
                    :str (make-node :core/string c)
                    :font (make-node :core/name fp)
                  })
                  [mode level] ])))

          ; TODO: handle primes and subscripts somehow?
          :view/expr/var
          (fn [n [mode level]]
            [ (make-node :view/chars {
                :str (as-string (node-attr n :str))
                :font (-> FONTS_BY_STYLE_AND_MODE :var mode)
                ; :font :timesItalic  ;; HACK
              })
              [mode level] ])
              
          :view/expr/int
          (fn [n [mode level]]
            [ (make-node :view/chars {
                :str (as-string (node-attr n :str))
                :font (-> FONTS_BY_STYLE_AND_MODE :int mode)
              })
              [mode level] ])

          :view/expr/string
          ; TODO: remove the quotes (they should be at language level)
          ; TODO: use grey for space indicator, etc.
          ; TODO: substitute for tab, return, etc. also
          (fn [n [mode level]]
            (let [val (node-attr n :str)
                  s (as-string val)
                  ds (.replace (str s) \space (.charAt "\u2423" 0)) ]  ; substitute for space chars
              [ (make-node :view/sequence [
                  (make-node :view/chars {
                    :str "\u005c"  ; Note: open and close quote chars for :cmr
                    :font :cmr10
                  })
                  (make-node :view/chars {
                    :str ds
                    :font (-> FONTS_BY_STYLE_AND_MODE :string mode)
                    ; :view/drawable/color (node :view/rgb :red 0 :green 0.5 :blue 0)
                  })
                  (make-node :view/chars {
                    :str "\""  ; Note: open and close quote chars for :cmr
                    :font :cmr10
                  })
                ])
                [mode level] ]))

          :view/expr/mono
          (fn [n [mode level]]
            [ (make-node :view/chars {
                :str (as-string (node-attr n :str))
                :font :courier
              })
              [mode level] ])
  
          :view/expr/prod
          (fn [n [mode level]]
            [ (make-node :view/chars {
                :str (as-string (node-attr n :str))
                :font :courierItalic
              })
              [mode level] ])
  
          :view/expr/doc
          (fn [n [mode level]]
            [ (make-node :view/chars {
                :str (as-string (node-attr n :str))
                :font :sans
                :view/drawable/color 
                (make-node :view/gray { :brightness 0.3 })
              })
              [mode level] ])
  
          :view/expr/prime
          (fn [n [mode level]]
            [ (make-node :view/expr/symbol { :str :prime })
              [mode level] ])
  
          :view/expr/missing
          (fn [n [mode level]]
            [ (make-node :view/chars {
                :str "?"
                :font :cmr10
                :view/drawable/color (synthetic-color-node)
              })
              [mode level] ])

          ; ; HACK: this is easier than actually implementing growable parens for now, but this
          ; ; node should really be handled in nodes.clj with custom rendering
          ; :view/parens
          ; (fn [n [mode level]]
          ;   [ (make-node :view/sequence [
          ;       (node :view/chars
          ;         :str (node-attr n :left)
          ;         :font :times; :cmr10
          ;         :view/drawable/color (with-attr n :view/drawable/color c c (black-node)))
          ;       (node-attr n :content)
          ;       (node :view/chars
          ;         :str (node-attr n :right)
          ;         :font :times; :cmr10
          ;         :view/drawable/color (with-attr n :view/drawable/color c c (black-node)))
          ;     ])
          ;     [mode level] ])
              
          :view/expr/embed
          (fn [n [mode level]]
            [(node :view/border
                :weight 1
                :margin 3
          
                :view/drawable/colors [ (nth EMBED_BORDER_COLORS (inc level)) ]
          
                :fill
                (nth EMBED_COLORS (inc level))
            
                :item
                (node-attr n :content))
              [mode (inc level)] ])
              
          :view/expr/disembed
          (fn [n [mode level]]
            (let [; _ (print-node n) _ (println "[m l]" [mode level])
                  inner (if (< level 1) ; HACK: this will fail if :levels is > 1
                          (do (println "warning: embedding error at node " (node-id n)) 1) 
                          level)
                  outer (- inner (node-attr-value n :levels))]
              [(node :view/border
                  :weight 1
                  :margin 3
            
                  :view/drawable/colors [ (nth EMBED_BORDER_COLORS inner) ]
            
                  :fill
                  (nth EMBED_COLORS outer)
              
                  :item
                  (node-attr n :content))
                [mode outer] ]))
                
            :view/over
            (fn [n [mode level]]
              (let [np (if (= (node-type (node-attr n :top)) :view/expr/temp-over)   ; HACK! to prevent infinite recursion
                          nil
                          (make-node :view/over {
                              :top
                              (make-node :view/expr/temp-over {
                                :body
                                (node-attr n :top)
                              })
                  
                              :weight
                              (node-attr n :weight)
                  
                              :bottom
                              (make-node :view/expr/temp-over {
                                :body
                                (node-attr n :bottom)
                              })
                            }))]
                  [ np [mode level] ]))  ; TODO: ???
                  
            :view/scripted
            (fn [n [mode level]]
              (let [np (if (= (node-type (node-attr n :super)) :view/expr/temp-super)   ; HACK! to prevent infinite recursion
                          nil
                          (make-node :view/scripted {
                            :nucleus
                            (node-attr n :nucleus)
                            
                            :super
                            (make-node :view/expr/temp-super {
                              :body
                              (node-attr n :super)
                            })
                          }))]
                [ np [mode level] ]))

          ; Handle temporary node by simple extracting the body and 
          ; adjusting the mode:
          :view/expr/temp-over
          (fn [n [mode level]]
            [ (make-node :view/sequence [ (node-attr n :body) ]) 
              [(fraction-mode mode) level] ])
          :view/expr/temp-super
          (fn [n [mode level]]
            [ (make-node :view/sequence [ (node-attr n :body) ]) 
              [(superscript-mode mode) level] ])
        }
      f (fn [n [mode level]]
          (if-let [r (rules (node-type n))]
            ; (let [x  ; HACK
            (r n [mode level])
            ; _ (println "n, x:" (node-type n) x)]  ; HACK
            ; x)  ; HACK
            [nil [mode level]]))
      [np o v] (reduce-plus n f [ :T 0 ])]
    [np o]))


;
; "Meta-reduction" for the view/expr language, used for rendering grammars, etc.
; This is a bit tricky in that the result of the reduction is new nodes in the 
; same grammar. To avoid infinite recursion, a set of ids of nodes to reduce is 
; threaded through the reduction. Each time a node is expanded, its id is removed
; from the set, so that when the descendants of the new node are reduced, the
; original node is not reduced again.
;

; ; reduceOnce :: node -> (node -> Maybe node) -> (node -> Maybe node)
; (defn reduceOnce
;   "Given a node and reduction, return a new reduction which applies the given 
;   function only to nodes of the original program (that is, it does _not_ 
;   recursively reduce nodes that are the result of a reduction)."
;   [n r]
;   (let [ids (set (deep-node-ids n))
;         _ (println "source ids:" ids)]
;     (fn [np] 
;       (if (ids (node-id np))
;         (do (println "reduce!" (node-id np)) (r np))
;         (do (println "no reduction" (node-id np)) nil)))))

; (def metaExprRules
;   (letfn [ (borderize [b title]
;               (fn [n]
;                 ; TODO: titled borders?
;                 (node :view/border
;                   :weight 1
;                   :margin 1
;     
;                   :view/drawable/colors [
;                     (node :view/gray :brightness b)
;                   ]
;     
;                   :item
;                   (rename-nodes n)))) ]  ; Tricky! need to rename the node being embedded in the result, so it won't be recursively reduced
;   {  
;     :view/juxt
;     (borderize 0.7 "juxt")
;   
;     :view/expr/binary
;     (borderize 0.7 "binary")
;   
;     :view/expr/relation
;     (borderize 0.7 "relation")
; 
;     :view/expr/flow
;     (borderize 0.7 "flow")
; 
;     :view/expr/keyword
;     (borderize 0.7 "kw")
; 
;     :view/expr/symbol
;     (borderize 0.7 "sym")
; 
;     :view/expr/var
;     (borderize 0.7 "var")
; 
;     :view/expr/int
;     (borderize 0.7 "int")
;       ; (node :view/chars
;       ;   :str (node-attr n :view/expr/int/str)
;       ;   :font :cmr10))
; 
;     :view/expr/string
;     (borderize 0.7 "str")
; 
;     :view/expr/mono
;     (borderize 0.7 "mono")
;   
;     :view/expr/prod
;     (borderize 0.7 "prod")
;   }))


;
; Reduction of :view/expr nodes. Each node is reduced only once, and only if 
; its id appears in the set which is provided as the reduction's aux. value.
;
(def metaExprRules
  (letfn [ (borderize 
              [b title value-attr]
              (fn [n v]
                (let [id (node-id n)]
                  (if (contains? v id)
                    (let [ val (if-not value-attr 
                                       n
                                       (let [a (node-attr n value-attr)]
                                         (if (value-node? a)
                                           n
                                           a)))
                           np (node :view/scripted
                                :nucleus
                                (node :view/border
                                  :weight 1
                                  :margin 1
    
                                  :view/drawable/colors [
                                    (node :view/gray :brightness b)
                                  ]
    
                                  :item
                                  val)
                                  
                                :super
                                (make-node :view/chars {
                                  :str title 
                                  :font :tiny
                                  :view/drawable/color (make-node :view/gray { :brightness b })
                                })) ]
                      [ np (disj v id) ])
                    [ nil v ]))))]
  {  
    :view/expr/juxt
    (borderize 0.7 "juxt" nil)
  
    :view/expr/binary
    (borderize 0.7 "bin" nil)
  
    :view/expr/relation
    (borderize 0.7 "rel" nil)

    :view/expr/flow
    (borderize 0.7 "flow" nil)

    :view/expr/keyword
    (borderize 0.7 "kw" :str)

    :view/expr/symbol
    (borderize 0.7 "sym" :str)

    :view/expr/var
    (borderize 0.7 "var" :str)

    :view/expr/int
    (borderize 0.7 "int" :str)
      ; (node :view/chars
      ;   :str (node-attr n :view/expr/int/str)
      ;   :font :cmr10))

    :view/expr/string
    (borderize 0.7 "str" :str)

    :view/expr/name
    (borderize 0.7 "name" :str)

    :view/expr/mono
    (borderize 0.7 "mono" :str)
  
    :view/expr/prod
    (borderize 0.7 "prod" :str)
    
    :view/expr/doc
    (borderize 0.7 "doc" :str)
    
    :view/expr/embed
    (borderize 0.7 "embed" :content)

    :view/expr/disembed
    (borderize 0.7 "disembed" :content)
  }))
