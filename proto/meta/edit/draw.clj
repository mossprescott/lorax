; Shell for the editor, including window management as well as all event 
; handling.

(ns meta.edit.draw
  (:use (clojure test)
        (meta core reduce)
        (meta.clojure kernel)
        (meta.edit nodes expr))
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
  
(set! *warn-on-reflection* true)

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

(def MARGIN 10)
(def SELECTED_COLOR 
  (Color. (float 1.0) (float 0.7) (float 0.85)))
(def BACKGROUND_COLOR
  Color/WHITE)
  ;(Color. (float 0.7) (float 0.7) (float 0.7)))
  ; (Color. (float 0.5) (float 0.5) (float 0.5)))


(def resolveOne) ; forward decl.

(defn drawNode
  "Recursively draw nodes, using nodes/draw and nodes/layout."
  [n #^Graphics2D g debug? selected errors o]
  ; selection hilite (behind the content):
  (if (selected (resolveOne (node-id n) o))
    (let [ [w h b] (size n g) ]
      (doto g
        (.setColor SELECTED_COLOR)
        (.setStroke (BasicStroke. 2))
        (.draw (Rectangle2D$Float. -2 -2 (+ w 4) (+ h 4)))))) ; TODO: align to pixels?
  ; the node's content:
  (draw n g debug?)
  ; the node's children:
  (doseq [ [child x y w h] (layout n g) ]
    (let [ gp (doto (.create g) (.translate x y)) ]
      (drawNode child gp debug? selected errors o))) ; no clipping for now  
      ; (drawNode child (.create g x y w h) debug? selected)))  ; clipping
  ; error indicator: 
  (if (errors (resolveOne (node-id n) o))
    (let [ [w h b] (size n g)
            y (if b (+ b 4) h) ]
      (doto g
        (.setColor Color/RED)
        (.setStroke (BasicStroke. 1))
        (.draw (Line2D$Float. 0.5 (+ y 0.5) w (+ y 0.5)))))))

; This is way out of hand. basically, it's a recursive search of the tree, building
; a list of containing nodes 
(defn findHitNodes
  "Given a node and (relative) coords x and y, returns a list of ids of nodes containing the 
  point, with the innermost node (if any) first."
  [n g x y]
  (let [find (fn find [n g x y lst]
                (let [ [w h b] (size n g) ]
                  (if (and (>= x 0) (>= y 0) (< x w) (< y h))
                    (let [lstp (conj lst (node-id n)) ]
                      (loop [nlst (layout n g)]
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
              (paintComponent [#^Graphics2D g]
                (let [this #^JComponent this]
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
                  (drawNode @nref (doto (.create g) (.translate MARGIN MARGIN)) @dref @sref errors @oref)))
              (getPreferredSize []
                (let [this #^JComponent this
                      g (.getGraphics this)
                      [w h b] (size @nref g)]
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
      
; Obsolete simpler display:
(defn makeFrame 
  [n title]
  (let [frame (JFrame. (str "Meta - " title))
        debugFlag (ref false)
        display (fn [n p] (meta-reduce (if p (parenthesize n) n) (reduceByType exprRules)))
        ; display (fn [n p] n) ; HACK
        nref (ref (display n false))
        sref (ref #{})  ; HACK
        panel #^JComponent (makePanel nref debugFlag, sref)
        scroll (JScrollPane. panel)
        parens (JCheckBox. "Parenthesize")
        debug (JCheckBox. "Show box outlines")]
    (.addActionListener parens 
      (proxy [ActionListener] []
        (actionPerformed [evt]
          (dosync (ref-set nref (display n (.isSelected parens))))
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

(defn makeSyntaxFrame 
  "primary: reduction to the 'expr' language
  errors: map of ids to seq of errors"
  [n title primary errors]
  (let [debugFlag (ref false)
        ; lastReduction (apply-until [reduceAny (reduceByType exprRules)])
        display (fn [n p] 
                  (let [ ; baz (do (print "n: ") (print-node n true))  ; HACK
                        [np o] (meta-reduce2 n primary)
                         ; foo (do (print "np: ") (print-node np))  ; HACK
                        ; bar (println "o:" o)  ; HACK
                        [npp op] (meta-reduce2 np reduceAny)
                         ; baz (do (print "npp: ") (print-node npp))  ; HACK
                        nppp (if p (parenthesize npp) npp)
                        [npppp opp] (meta-reduce2 nppp (reduceByType exprRules)) ]
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
        left (JButton. (ImageIcon. "meta/edit/Back24.gif"))
        right (JButton. (ImageIcon. "meta/edit/Forward24.gif"))
        parens (doto (JCheckBox. "Parens") 
                  (.setSelected PARENS_DEFAULT))
        debug (JCheckBox. "Outlines")
        header (doto (JToolBar.)
                (.setFloatable false)
                (.add left)
                (.add right)
                (.add parens)
                (.add debug))
        [#^JComponent inspector inspectorUpdate] (makeInspectorPanel snref)
        frame (doto (JFrame. (str "Meta - " title))
                (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
                (.add scroll)
                (.add header "North")
                (.add inspector "South")
                (.setSize 500 750)
                (.setVisible true))
        updateSelection (fn [sourceId]
                          (dosync 
                            (ref-set sref #{sourceId})
                            (ref-set snref (find-node n sourceId))
                          (.repaint panel)
                          (inspectorUpdate)))]
                          
    ; move selection left and right:
    ; TODO: put the nodes into the order that their reductions appear
    (.addActionListener left
      (proxy [ActionListener] []
        (actionPerformed [evt]
          (if-not (seq @sref)
            (updateSelection (node-id n))
            (let [allIds (deep-node-ids n)
                  cur (indexOf (first @sref) allIds)]
              (updateSelection (nth allIds (dec cur))))))))
    (.addActionListener right
      (proxy [ActionListener] []
        (actionPerformed [evt]
          (if-not (seq @sref)
            (updateSelection (node-id n))
            (let [allIds (deep-node-ids n)
                  cur (indexOf (first @sref) allIds)]
              (updateSelection (nth allIds (inc cur))))))))
              
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
            (updateSelection firstSourceId)))))
    nil))

(defn makeKernelFrame 
  [n title errors]
  (makeSyntaxFrame n title (reduceByType kernelPresRules) errors))
