; Shell for the editor, including window management as well as all event 
; handling.

(ns meta.edit.draw
  (:use (clojure test)
        (meta core reduce path)
        (meta.clojure kernel)
        (meta.edit nodes expr select))
  (:import 
    (javax.swing
      ImageIcon 
      JButton 
      JCheckBox
      JComponent 
      JFrame 
      JLabel
      JPanel
      JScrollPane
      JTextField
      JToolBar)
    (java.awt.event 
      ActionListener
      KeyAdapter
      KeyEvent
      MouseAdapter
      MouseEvent)
    (java.awt 
      Color 
      Dimension 
      GridLayout 
      RenderingHints 
      Graphics2D 
      BasicStroke)
    (java.awt.geom 
      Line2D 
      Line2D$Float
      Rectangle2D$Float)))
  
; (defn celsius []
;   (let [frame (JFrame. "Celsius Converter")
;         temp-text (JTextField.)
;         celsius-label (JLabel. "Celsius")
;         convert-button (JButton. "Convert")
;         fahrenheit-label (JLabel. "Fahrenheit")]
;     (.addActionListener convert-button
;       (proxy [ActionListener] []
;         (actionPerformed [evt]
;           (let [c (Double/parseDouble (.getText temp-text))]
;             (.setText fahrenheit-label
;                (str (+ 32 (* 1.8 c)) " Fahrenheit"))))))
;     (doto frame
;       (.setLayout (GridLayout. 2 2 3 3))
;       (.add temp-text)
;       (.add celsius-label)
;       (.add convert-button)
;       (.add fahrenheit-label)
;       (.setSize 300 80)
;       (.setVisible true))))
; (celsius)

(def PRINT_ALL false)

(def MARGIN 10)
; (def SELECTED_COLOR 
;   (Color. (float 1.0) (float 0.7) (float 0.85)))
(def BACKGROUND_COLOR
  Color/WHITE)
  ;(Color. (float 0.7) (float 0.7) (float 0.7)))
  ; (Color. (float 0.5) (float 0.5) (float 0.5)))


(def resolveOne) ; forward decl.

(defn draw-node
  "Recursively draw nodes, using nodes/draw and nodes/layout."
  [n ^Graphics2D g ctx df]
  (if (not (node? n))
    (println "not a node: " n)
    (do
      ; the node's content:
      (df n g)
      ; the node's children:
      (doseq [ [child x y w h] (layout n g ctx) ]
        (let [ gp (doto (.create g) (.translate x y)) ]  ; TODO: eliminate extra graphics creation?
          (draw-node child gp ctx df)))))) ; no clipping for now  
          ; (draw-node child (.create g x y w h) df)))  ; clipping

(defn find-child-rects
  "Find locations of the nearest descendents of node n which represent nodes
  of the source program."
  [n g o ctx]
  (apply concat 
    (for [ [c x y w h] (layout n g ctx) ]
      (if (resolveOne (node-id c) o)
        [ [x y w h] ]
        (for [ [xx yy ww hh] (find-child-rects c g o ctx)]
          [(+ x xx) (+ y yy) ww hh])))))

(defn draw-all
  "Draw the program by making a series of traversals, painting the nodes and 
  other visual elements in layers, effectively. Because the layout calculations
  are memoized in the drawing context, this isn't _too_ silly.
  Nodes are always drawn on top of their ancestors.
  Note: a simple hack (drawing the :view/border nodes in the lowest layer) 
  puts the the background colors underneath everything else.
  Params:
  selected - set of ids of selected source nodes"
  [root g debug? selected errors o]
  (let [ctx (make-draw-context)]
    (do
      ; borders:
      (draw-node root g ctx
        (fn [n g]
          (if (= :view/border (node-type n))
            (draw n g ctx debug?))))
      ; selection hilite (behind the content):
      (draw-node root g ctx
        (fn [n ^Graphics2D g]
          (if (selected (resolveOne (node-id n) o))
            (let [ [w h b] (size n g ctx) 
                   cs (find-child-rects n g o ctx)]
              (draw-selection-hilite g w h cs)))))
      ; node content:
      (draw-node root g ctx
        (fn [n g]
          (if (not= :view/border (node-type n))
            (draw n g ctx debug?)))))))
  ;       ; error indicator: 
  ;       (if (errors (resolveOne (node-id n) o))
  ;         (let [ [w h b] (size n g)
  ;                 y (if b (+ b 4) h) ]
  ;           (doto g
  ;             (.setColor Color/RED)
  ;             (.setStroke (BasicStroke. 1))
  ;             (.draw (Line2D$Float. 0.5 (+ y 0.5) w (+ y 0.5)))))))))

; (defn drawNode
;   "Recursively draw nodes, using nodes/draw and nodes/layout."
;   [n #^Graphics2D g debug? selected errors o]
;   (if (not (node? n))
;     (println "not a node: " n)
;     (do
;       ; selection hilite (behind the content):
;       (if (selected (resolveOne (node-id n) o))
;         (let [ [w h b] (size n g) ]
;           (doto g
;             (.setColor SELECTED_COLOR)
;             (.setStroke (BasicStroke. 2))
;             (.draw (Rectangle2D$Float. -2 -2 (+ w 4) (+ h 4)))))) ; TODO: align to pixels?
;       ; the node's content:
;       (draw n g debug?)
;       ; the node's children:
;       (doseq [ [child x y w h] (layout n g) ]
;         (let [ gp (doto (.create g) (.translate x y)) ]
;           (drawNode child gp debug? selected errors o))) ; no clipping for now  
;           ; (drawNode child (.create g x y w h) debug? selected)))  ; clipping
;       ; error indicator: 
;       (if (errors (resolveOne (node-id n) o))
;         (let [ [w h b] (size n g)
;                 y (if b (+ b 4) h) ]
;           (doto g
;             (.setColor Color/RED)
;             (.setStroke (BasicStroke. 1))
;             (.draw (Line2D$Float. 0.5 (+ y 0.5) w (+ y 0.5)))))))))

; This is way out of hand. Basically, it's a recursive search of the tree, building
; a list of containing nodes 
(defn findHitNodes
  "Given a node and (relative) coords x and y, returns a list of ids of nodes containing the 
  point, with the innermost node (if any) first."
  [n g x y]
  (let [ctx (make-draw-context)
        find (fn find [n g x y lst]
                (let [ [w h b] (size n g ctx) ]
                  (if (and (>= x 0) (>= y 0) (< x w) (< y h))
                    (let [lstp (conj lst (node-id n)) ]
                      (loop [nlst (layout n g ctx)]
                        (if (empty? nlst) 
                          lstp
                          (let [ [n nx ny nw nh] (first nlst) ]
                            (let [lstpp (find n g (- x nx) (- y ny) lstp)]
                              (if (empty? lstpp)
                                (recur (rest nlst))
                                lstpp))))))
                    ())))]          
      (find n g x y ())))
  
(defn makePanel
  "Makes a component which draws the node in nref, with the debug flag in dref,
  and drawing a hilite box behind every node whose id appears in the set sref."
  [nref dref sref errors oref]
  (let [ c (proxy [ JComponent ] []
              (paintComponent [^Graphics2D g]
                (let [this ^JComponent this]
                  (doto g
                    (.setColor BACKGROUND_COLOR)
                    (.fillRect 0 0 (.getWidth this) (.getHeight this))
          
                    (.setRenderingHint RenderingHints/KEY_ANTIALIASING 
                                        RenderingHints/VALUE_ANTIALIAS_ON)
                    (.setRenderingHint RenderingHints/KEY_STROKE_CONTROL 
                                        RenderingHints/VALUE_STROKE_PURE)
                    ; This doesn't seem to work; I still have only grayscale aa afaict
                    (.setRenderingHint RenderingHints/KEY_TEXT_ANTIALIASING
                                        RenderingHints/VALUE_TEXT_ANTIALIAS_LCD_HRGB) )
                                        
                  ; (println "layout" (layout (node :view/sequence :items [ (node :view/chars :str "abc" :font :cmr10) (node :view/chars :str "a" :font :cmmi10) ]) 
                  ;                                   (.getGraphics this))) ; HACK
                  ;         (println "size" (size (node :view/sequence :items [ (node :view/chars :str "abc" :font :cmr10) (node :view/chars :str "a" :font :cmmi10) ]) 
                  ;                                   (.getGraphics this))) ; HACK
                  (print "draw-all: ")
                  (time (draw-all @nref (doto (.create g) (.translate MARGIN MARGIN)) @dref @sref errors @oref))))
              (getPreferredSize []
                (let [this #^JComponent this
                      g (.getGraphics this)
                      ctx (make-draw-context)
                      [w h b] (size @nref g ctx)]
                  (Dimension. (+ MARGIN w MARGIN) (+ MARGIN h MARGIN))))) ]
      ; (.addMouseListener c 
      ;   (proxy [ MouseAdapter ] []
      ;     (mouseClicked [#^MouseEvent evt]
      ;       ; (println "clicked:" evt)
      ;       (let [ s (findHitNodes @nref (.getGraphics c) 
      ;                   (- (.getX evt) MARGIN) 
      ;                   (- (.getY evt) MARGIN))
      ;               ss (if (empty? s) #{} #{(first s)}) ]
      ;         ; (println "hit:" s)
      ;         (dosync (ref-set sref ss)))
      ;       (.repaint c))))
      c))
      

(defn makeInspectorPanel
  "Panel containing information about the selected node, if any. What I really 
  need for this stuff is a nice DSL for GUI...
  Returns a panel and a no-arg fxn to be invoked when the view needs to be 
  updated. Would an atom and a watcher be simpler?"
  [nref]
  (let [l (JLabel.)
        update (fn [] 
                (let [n @nref]
                  (.setText l 
                    (if n
                      (str (node-type n) " (" (node-id n) ")")
                      " "))))]
    (update)
    [l update]))
  
   
(defn makeErrorPanel
  "Panel containing a table of errors."
  [] ; TODO
  nil) ; TODO
      
; Obsolete simpler display for :view/expr programs only:
(defn makeFrame 
  [n title]
  (let [frame (JFrame. (str "Meta - " title))
        debugFlag (ref false)
        display (fn [n p] (exprToView (if p (parenthesize n) n)))
        ; display (fn [n p] n) ; HACK
        [n o] (display n false)
        nref (ref n)
        _ (if PRINT_ALL (print-node @nref true))
        sref (ref #{})  ; HACK
        panel #^JComponent (makePanel nref debugFlag sref {} (ref {}))
        scroll (JScrollPane. panel)
        parens (JCheckBox. "Parenthesize")
        debug (JCheckBox. "Show box outlines")]
    (.addActionListener parens 
      (proxy [ActionListener] []
        (actionPerformed [evt]
          (let [ [n o] (display n (.isSelected parens))]
            (dosync (ref-set nref n)))
          (.repaint panel))))
    (.addActionListener debug 
      (proxy [ActionListener] []
        (actionPerformed [evt]
          (dosync (ref-set debugFlag (.isSelected debug)))
          (.repaint panel))))
    (doto frame
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.add scroll)
      (.add parens "North")
      (.add debug "South")
      (.setSize 480 240)
      (.setVisible true))))

(defn resolveOne
  "Look up a key in a series of maps, using the value from each map
  as a key in the succesive map, and returning the final value. If
  any map has no such key, then nil.
  Seems like there would be a simpler way."
  [v ms]
  (if (nil? (seq ms))
    v
    (if-let [vp ((first ms) v)]
      (resolveOne vp (rest ms)))))

(deftest resolveOne1
  (is (= nil (resolveOne :a '({:b :c}))))
  (is (= :a (resolveOne :a '())))
  (is (= :c (resolveOne :a '({:a :b} {:b :c})))))

(defn resolvePath
  "Given a list of ids and a list of maps, returns a list of [id, resolved], where
  each id has been resolved through each map in turn. If any id is missing in
  any map, the result is shortened."
  [path os]
  (filter #(not (nil? (second %))) 
    (for [id path] [id (resolveOne id os)])))

(deftest resolvePath1
  (is (= '([:a :a] [:b :b] [:c :c]) 
        (resolvePath '(:a :b :c) 
                      '())))
  (is (= '([:a :d] [:b :e]) 
        (resolvePath '(:a :b :c) 
                      '({:a :d, :b :e}))))
  (is (= '([:b :f]) 
        (resolvePath '(:a :b :c) 
                      '({:a :d, :b :e}
                        {:e :f})))))


(defn indexOf
  [x coll]
  (loop [i 0 s (seq coll)]
    (cond 
      (nil? (seq s)) nil
      (= (first s) x) i
      true (recur (inc i) (rest s)))))


(def PARENS_DEFAULT true)

;
; Selection...
;

; From somebody's article on Clojure:
(defn indexed 
  "Sequence of pairs [index, element] of the collection."
  [coll] 
  (map vector (iterate inc 0) coll))  
(defn index-filter 
  "Sequence of indexes of elements for which the predicate is true."
  [pred coll]
  (when pred 
    (for [[idx elt] (indexed coll) :when (pred elt)] idx)))


(defn- idToPath
  [id root]
  (first (filter 
    #(let [n (node-at-path root %)]
        (and (node? n)  ; TODO: deal with sequences
             (= id (node-id n))))
    (node-paths root))))

(defn- sortAs
  "Sequence of the elements of _coll_, in the order that they appear in 
  _sorted_."
  [coll sorted]
  (let [scoll (set coll)]
    (filter #(contains? scoll %) sorted)))
    
(deftest sortAsTest
  (is (= [1 2 3]
        (sortAs [3 1 2] (range 20))))
  (is (= [1 2 3]
        (sortAs [-1 1 100 3 50 2] (range 20)))))

(defn- siblings
  [sourceId root sourceIdsInViewOrder]
  (if-let [p (idToPath sourceId root)]
    (let [paths (sibling-paths root p)
          ids (for [p paths] (node-id (node-at-path root p)))]
      (sortAs ids sourceIdsInViewOrder))))
  
(defn- leftIdOrNil
  [sourceId root sourceIdsInViewOrder]
  (if-let [ids (siblings sourceId root sourceIdsInViewOrder)]
    (let [i (first (index-filter #(= sourceId %) ids))]
      (if (and i (> i 0))
        (nth ids (dec i))))))

(defn- rightIdOrNil
  [sourceId root sourceIdsInViewOrder]
  (if-let [ids (siblings sourceId root sourceIdsInViewOrder)]
    (let [i (first (index-filter #(= sourceId %) ids))
          n (count ids)]
      (if (and i (< i (dec n)))
        (nth ids (inc i))))))

(defn- parentIdOrNil
  [sourceId root sourceIdsInViewOrder]
  (let [p (idToPath sourceId root)]
    (if (not (root-path? p))
      (let [pn (node-at-path root (parent-path p))]
        (if (node? pn)  ; TODO: deal with sequences
            (node-id pn))))))

(defn- firstChildIdOrNil
  [sourceId root sourceIdsInViewOrder]
  (let [p (idToPath sourceId root)
        cs (child-paths root p)]
    (if cs
      (let [c (first cs)  ; TODO: order by position in the reduced program
            cn (node-at-path root c)]
        (if (node? cn)
          (node-id cn))))))

(defn makeSyntaxFrame 
  "primary: reduction to the 'expr' language
  errors: map of (source program) ids to seq of errors"
  [n title primary errors]
  (let [debugFlag (ref false)
        ; lastReduction (apply-until [reduceAny (reduceByType exprRules)])
        display (fn [n p] 
                  (let [ _ (if PRINT_ALL (do (print "source: ") (print-node n true)) )
                        [np o] (primary n)
                        _ (if PRINT_ALL (do (print "expr: ") (print-node np true) 
                                            (println "o:" o)) )
                        [npp op] (meta-reduce2 np reduceAny)
                        _ (if PRINT_ALL (do (print "general: ") (print-node npp true)) )
                        nppp (if p (parenthesize npp) npp)
                        _ (if PRINT_ALL (do (print "parens: ") (print-node nppp true)) )
                        [npppp opp] (exprToView nppp)
                        _ (if PRINT_ALL (do (print "view: ") (print-node npppp true)) ) ]
                    ; (print-node nppp true)  ; HACK
                    ; (println "foo")  ; HACK
                    [npppp [opp op o]]))
        [np o] (display n PARENS_DEFAULT) 
        nref (ref np)  ; contains the reduced program
        oref (ref o)   ; contains a list of maps of reduced program node ids to pre-reduction ids
        sref (ref #{}) ; contains a set of selected node (reduced program) ids
        snref (ref nil) ; the actual selected node
        panel #^JComponent (makePanel nref debugFlag sref (set (keys errors)) oref)
        scroll (JScrollPane. panel)
        
        [#^JComponent inspector inspectorUpdate] (makeInspectorPanel snref)

        updateSelection (fn [sourceId]
                          (dosync 
                            (ref-set sref #{sourceId})
                            (ref-set snref (find-node n sourceId))
                          (.repaint panel)
                          (inspectorUpdate)))
                          
        selectionButton (fn [f #^String iconPath]
                          (doto (JButton. (ImageIcon. iconPath))
                            (.addActionListener
                              (proxy [ActionListener] []
                                (actionPerformed [evt]
                                  (if (seq @sref)
                                    (let [sourceIdsInViewOrder (map #(resolveOne % @oref) (deep-node-ids @nref))
                                          id (f (first @sref) n sourceIdsInViewOrder)]
                                      (if id
                                        (updateSelection id)))))))))

        #^JButton left (selectionButton leftIdOrNil "meta/edit/image/Back16.gif")
        #^JButton right (selectionButton rightIdOrNil "meta/edit/image/Forward16.gif")
        #^JButton up (selectionButton parentIdOrNil "meta/edit/image/Up16.gif")
        #^JButton down (selectionButton firstChildIdOrNil "meta/edit/image/Down16.gif")
        selectionToolBar (doto (JToolBar.)
                          (.add (JLabel. "Selection:"))
                          (.add left)
                          (.add right)
                          (.add up)
                          (.add down))
        
        parens (doto (JCheckBox. "Parens") 
                  (.setSelected PARENS_DEFAULT))
        debug (JCheckBox. "Outlines")
        optionsToolBar (doto (JToolBar.)
                        (.add (JLabel. "Options:"))
                        (.add parens)
                        (.add debug))

        header (doto (JPanel.)
                (.setLayout (GridLayout. 0 1))
                (.add selectionToolBar)
                (.add optionsToolBar))
                        
        frame (doto (JFrame. (str "Meta - " title))
                (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
                (.add scroll)
                (.add header "North")
                (.add inspector "South")
                (.setSize 500 750)
                (.setVisible true))]

    (.addActionListener parens 
      (proxy [ActionListener] []
        (actionPerformed [evt]
          (let [ [n o] (display n (.isSelected parens)) ]
            (dosync 
              (ref-set nref n)
              (ref-set oref o)))
          (.repaint panel))))
    (.addActionListener debug 
      (proxy [ActionListener] []
        (actionPerformed [evt]
          (dosync (ref-set debugFlag (.isSelected debug)))
          (.repaint panel))))

    (.addMouseListener panel 
      (proxy [ MouseAdapter ] []
        (mouseClicked [#^MouseEvent evt]
          ; (println "clicked:" evt)
          (let [ hitPath (findHitNodes @nref (.getGraphics panel) 
                      (- (.getX evt) MARGIN) 
                      (- (.getY evt) MARGIN))
                  hitSourcePath (resolvePath hitPath @oref)
                  [firstId firstSourceId] (if (empty? hitSourcePath) 
                                            [nil nil] (first hitSourcePath)) ]
            ; (println "o:" @oref)
            ; (println "hit:" hitPath hitSourcePath firstSourceId)
            (updateSelection firstSourceId)
            (.requestFocus panel)  ; ???
            ))))

    (.addKeyListener panel
      (proxy [ KeyAdapter ] []
        (keyPressed [#^KeyEvent evt]
          ; (let [k (.getKeyCode evt)]
          ;   (println "pressed:" evt)
            (condp = (.getKeyCode evt)
              KeyEvent/VK_LEFT (.doClick left)
              KeyEvent/VK_RIGHT (.doClick right)
              KeyEvent/VK_UP (.doClick up)
              KeyEvent/VK_DOWN (.doClick down)
              nil))))
    nil))

; (defn makeKernelFrame 
;   [n title errors]
;   (makeSyntaxFrame n title #(meta-reduce2 % (reduceByType kernelPresRules)) errors))
