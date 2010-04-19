; Functions over the low-level editor/presentation language, inspired by TeX's 
; horizontal list and math list concepts.

; TODO: grammar for the nodes
; TODO: optionally align certain elements to pixel (or half-pixel) boundaries?
; TODO: some kind of magnification factor?

(ns meta.edit.nodes
	(:use (meta core))
	(:import (java.awt 
	            BasicStroke
	            Color 
	            Font 
	            Graphics2D))
	(:import (java.awt.geom 
	            Line2D$Float 
	            Rectangle2D$Float)))
	
; Nodes:

; :view/color

; :view/rgb ::= color
; - /alpha: [0,1]?
; - /red: [0,1]
; - /green: [0,1]
; - /blue: [0,1]

; :view/gray ::= color (or just HSV instead?)
; - /alpha: [0,1]?
; - /brightness: [0,1]

; :view/drawable

; A sequence of characters, using normal text spacing:
; :view/chars ::= drawable
; - /str: Unicode
; - /font: fontName
; - /color: color?

; Space of differeing sizes:
; :view/thinSpace
; :view/mediumSpace
; :view/thickSpace
; :view/quad
; TODO: a flag for suppressing spaces in script/scriptscript mode

; A pair of parentheses surrounding a sub-expression:
; :view/parens ::= drawable 
; - /left: Unicode[0..1] (special handling of '([{', angle, floor, ciel, ...)
; - /right: Unicode[0..1] (ditto)
; - /content: drawable
; - /color: color?

; A group of nodes arranged in a horizontal sequence (hbox):
; :view/sequence ::= drawable
; - /items: drawable*

; A group of nodes arranged in a series of lines, aligned left (vbox):
; :view/section ::= drawable
; - /lines: drawable*
; - /indent: int (>= 0)

; Surrounds its content with a colored box, and draws a background behind it:
; :view/box ::= drawable
; - /fill: color?
; - /border:  color?
; - /borderStyle: raised/lowered/solid/none (something nicer?)
; - /borderWeight: width in pixels
; - /margin: space in pixels inside the border

; A distinctive underline, for indicating errors. Does not consume any space of its own.
; :view/squiggle ::= drawable
; - /color: color

; Some random constants:
; (def DEBUG false)
(def DEBUG_BOX_COLOR (Color. (float 0.0) (float 1.0) (float 0.0) (float 0.5)))

(def DEFAULT_COLOR Color/BLACK)

;
; jsMath's Computer Modern fonts:
;

(def DISPLAY_SIZE 18)
(def SCRIPT_SIZE (* DISPLAY_SIZE 0.7))
(def SCRIPT_SCRIPT_SIZE (* DISPLAY_SIZE 0.5))

(def FONTS {
  ; extended symbols
  :cmex10 (Font. "jsMath-cmex10" Font/PLAIN DISPLAY_SIZE)

  ; math italics
  :cmmi10 (Font. "jsMath-cmmi10" Font/PLAIN DISPLAY_SIZE)

  ; math roman
  :cmr10 (Font. "jsMath-cmr10" Font/PLAIN DISPLAY_SIZE)
  :cmr10-script (Font. "jsMath-cmr10" Font/PLAIN SCRIPT_SIZE)

  ; bold extended (keywords)
  :cmbx10 (Font. "jsMath-cmbx10" Font/PLAIN DISPLAY_SIZE)

  ; math symbol
  ; For some reason, glyphs do not appear for any char < 0x2x,
  ; although the jsMath site claims they are present.
  :cmsy10 (Font. "jsMath-cmsy10" Font/PLAIN DISPLAY_SIZE)
  
  ; Times, a unicode font...
  ; Unfortunately the spacing is quite different than the jsMath fonts
  :times (Font. "Times New Roman Regular" Font/PLAIN 14)
  :timesItalic (Font. "Times New Roman" Font/ITALIC DISPLAY_SIZE)
  
  :courier (Font. "Courier New" Font/PLAIN DISPLAY_SIZE)
  :courierItalic (Font. "Courier New" Font/ITALIC DISPLAY_SIZE)
})

;
; Color interpretation:
;
(defmulti color
  "Multi-method that takes :view/color node and returns an AWT Color instance."
  node-type)
  
(defmethod color :view/gray
  [n]
  (let [b (float (node-attr n :view/gray/brightness))]
    (Color. b b b)))  ; better way to get the right constructor?

(defmethod color :view/rgb
  [n]
  (Color. (float (node-attr n :view/rgb/red))
          (float (node-attr n :view/rgb/green))
          (float (node-attr n :view/rgb/blue))))

(defn node-color 
  "Takes a node which may or may not have a color attr, and returns an AWT Color 
  for drawing it."
  [n]
  (if-let [cn (n :view/drawable/color)]
    (color cn)
    DEFAULT_COLOR))

;
; Multi-methods for layout and drawing:
;

(defmulti size 
  "Multi-method calculating the size (in pixels) of a drawable.
  Takes [node g] and returns a vector of floats [width height baseline].
  If the node should not be positioned relative to the baseline, it is nil."
  (fn [n gfx] (node-type n)))

(defmulti layout
  "Returns a list of vectors [node x y w h] giving the bounds of each child
  (if any), relative to node's upper left corner."
  (fn [n gfx] (node-type n)))

(defmulti draw 
  "Multi-method drawing just the visible portions of a node (that is, 
  not the children), assuming the upper-left corner is located at the origin."
  (fn [n gfx debug?] (node-type n)))

;
; Chars:
;

(defmethod size :view/chars
  [n #^Graphics2D g]
  (let [s (node-attr n :view/chars/str)
        f (node-attr n :view/chars/font)
        fm (.getFontMetrics g (FONTS f))
        bounds (.getStringBounds fm s g)]
      ; (println bounds)
      [(.getWidth bounds)
        (if (= f :cmsy10)  ; HACK: workaround weird descent in the symbol font for now
          (- (.getMinY bounds))
          (.getHeight bounds))
        (- (.getMinY bounds))]))

(defmethod layout :view/chars [n #^Graphics2D g] [])

(defmethod draw :view/chars
  [n #^Graphics2D g debug?]
  (let [ [w h b] (size n g)
          font (FONTS (n :view/chars/font))
          s (node-attr n :view/chars/str)
          angle 0.0 ];(.getItalicAngle font) ]
    ; (println [s w h b angle])
    (if debug?
      (let [ y0 0 
              y1 b
              y2 h
              x00 (* angle b)
              x01 (+ x00 w)
              x10 0
              x11 (+ x10 w)
              x20 (- (* angle (- h b)))
              x21 (+ x20 w) ]
        (doto g
          (.setColor DEBUG_BOX_COLOR)
          ; (.drawRect x y w h)
          ; (.draw (Line2D$Float. (* angle b) 0 
          ;                       (+ w (* angle b)) 0))  ; above
          ; (.draw (Line2D$Float. 0 b w b))  ; baseline
          ; (.draw (Line2D$Float. (* angle (- b h)) h 
          ;                       (+ w (* angle (- b h))) h))))  ; below
          (.draw (Line2D$Float. x00 y0 x01 y0))  ; above
          (.draw (Line2D$Float. x10 y1 x11 y1))  ; baseline
          (.draw (Line2D$Float. x20 y2 x21 y2)))))  ; below
    (doto g 
      (.setColor (node-color n))
      (.setFont font)
      (.drawString s (float 0) (float b))
      )))

;
; Space:
;
(def QUAD_WIDTH (* 1.0 DISPLAY_SIZE))  ; HACK: this is the quad width for cmmi

(defmethod size :view/thinspace [n & more] 
  (let [ quadWidth QUAD_WIDTH ]
  [(* quadWidth (float 1/6)) 0 0]))  ; see Knuth, p167
(defmethod layout :view/thinspace [n & more] [])
(defmethod draw :view/thinspace [n & more] nil)

(defmethod size :view/mediumspace [n & more]
  (let [ quadWidth QUAD_WIDTH ]
  [(* quadWidth (float 2/9)) 0 0]))  ; see Knuth, p167
(defmethod layout :view/mediumspace [n & more] [])
(defmethod draw :view/mediumspace [n & more] nil)

(defmethod size :view/thickspace [n & more]
  (let [ quadWidth QUAD_WIDTH ]
  [(* quadWidth (float 5/18)) 0 0]))  ; see Knuth, p167
(defmethod layout :view/thickspace [n & more] [])
(defmethod draw :view/thickspace [n & more] nil)

(defmethod size :view/quad [n & more]
  ; TODO: calculate from the "current" font?
  [ QUAD_WIDTH 0 0 ])
(defmethod layout :view/quad [n & more] [])
(defmethod draw :view/quad [n & more] nil)

;
; Scripted:
;

(defmethod size :view/scripted
  [n #^Graphics2D g]
  (let [nucl (node-attr n :view/scripted/nucleus)
        [_ _ nb] (size nucl g)
        [[nucl nx ny nw nh] [sup sx sy sw sh]] (layout n g)]
    [(+ sx sw) (+ ny nh) (+ ny nb)]))

(defmethod layout :view/scripted 
  [n #^Graphics2D g]
  (let [nucl (node-attr n :view/scripted/nucleus)
        sup (node-attr n :view/scripted/super)
        [nx ny nb] (size nucl g)
        [sx sy sb] (size sup g)]
    [ [nucl 0 (/ sy 3) nx ny] [sup (+ 1 nx) 0 sx sy] ]))  ; HACK

(defmethod draw :view/scripted [n & more])


;
; Sequence (horizontal list):
;

(defmethod size :view/sequence
  ; height is the larger of max ascent + max descent, or max height
  [n #^Graphics2D g]
  (let [items (n :view/sequence/items)]
    (if (empty? items)
      [0 0 0]
      (let [szs (map #(size % g) items)
            maxAsc (apply max 0 (for [ [w h b] szs :when b ] b))
            maxDesc (apply max 0 (for [ [w h b] szs :when b ] (- h b)))
            maxHeight (apply max 0 (for [ [w h b] szs ] h))
            maxAD (+ maxAsc maxDesc)
            w (reduce + 0 (for [ [w h b] szs ] w))
            h (max maxAD maxHeight)
            top (- (/ h 2) (/ maxAD 2))
            b (if (= maxAsc 0) nil (+ top maxAsc))]
        [ w h b ]))))

(defmethod layout :view/sequence
  ; Align baselines, and vertically center any nodes without baselines.
  [n #^Graphics2D g] 
  (let [ [w h b] (size n g)]
    (loop [ items (n :view/sequence/items) 
            x 0 
            result [] ]
        (if (empty? items)
          result
          (let [d (first items)
                ds (rest items)
                [dw dh db] (size d g)
                y (if (and b db) 
                    (- b db)        ; use baseline
                    (/ (- h dh) 2)) ; center
                resultp (conj result [ d x y dw dh ])]
              (recur ds (+ x dw) resultp))))))

(defmethod draw :view/sequence [n #^Graphics2D g debug?] [])

;
; Section (vertical list):
;
(def LINE_SPACING 5)  ;; TODO: remove

(defmethod size :view/section
  [ n #^Graphics2D g]
  (let [items (node-attr n :view/section/items)
        szs (map #(size % g) items)]
    ; (println "n") (print-node n) ; HACK
    ; (println "szs" szs) ; HACK
    [ (apply max 0 (for [ [w h b] szs ] w))
      (- (reduce #(+ %1 (second %2) LINE_SPACING) 0 szs) LINE_SPACING)
      nil ])) 

(defmethod layout :view/section
  [n #^Graphics2D g] 
  (let [ [w h b] (size n g)]
    (loop [ items (node-attr n :view/section/items) 
            y 0 
            result [] ]
        (if (empty? items)
          result
          (let [d (first items)
                ds (rest items)
                [dw dh db] (size d g)
                resultp (conj result [ d 0 y dw dh ])]
      ;       ; (println "d ds" d ds)
              (recur ds (+ y dh LINE_SPACING) resultp))))))

(defmethod draw :view/section [n #^Graphics2D g debug?] [])


;
; Border:
;

(defmethod size :view/border
  [n #^Graphics2D g]
  (let [i (node-attr n :view/border/item)
        weight (node-attr n :view/border/weight)
        margin (node-attr n :view/border/margin)
        [w h b] (size i g)]
    [ (+ weight margin w margin weight) 
      (+ weight margin h margin weight) 
      (if b 
        (+ weight margin b) 
        nil)]))
    
(defmethod layout :view/border
  [n #^Graphics2D g]
  (let [i (node-attr n :view/border/item)
        weight (node-attr n :view/border/weight)
        margin (node-attr n :view/border/margin)
        [w h b] (size i g)]
    [ [i (+ weight margin) (+ weight margin) w h] ]))
    
(defmethod draw :view/border
  [n #^Graphics2D g debug?]
  (let [ [w h b] (size n g) 
          weight (node-attr n :view/border/weight) 
          i (if (odd? weight) 0.5 0)  ; an adjustment to make it align to pixels usually
          x2 (+ i (int w))
          y2 (+ i (int h))
          colors (for [c (node-attr n :view/drawable/colors)] (color c))
          ; foo (println colors)
          ]
    (if (= 1 (count colors))
      (doto g
        (.setStroke (BasicStroke. weight))
        (.setColor (nth colors 0))
        (.draw (Rectangle2D$Float. i i (int w) (int h))))
      (doto g
        (.setStroke (BasicStroke. weight))
        (.setColor (nth colors 0))
        (.draw (Line2D$Float. i i x2 i))
        (.draw (Line2D$Float. i i i y2))
        (.setColor (nth colors 1))
        (.draw (Line2D$Float. x2 i x2 y2))
        (.draw (Line2D$Float. i y2 x2 y2))))))


;
; Last-resort reduction:
;

(defn- reduceOne
  [v]
  (cond 
    (node? v) v

    (vector? v) (node :view/section 
                  :items 
                  (vec (map reduceOne v)))

    (string? v) (node :view/expr/string :str v)

    true (node :view/expr/keyword :str (str v))))


(defn reduceAny
  "Reduction which converts any non-view node, using a common presentation 
  which shows all children."
  [n]
  (let [typ (subs (str (node-type n)) 1)]
    ; (println "typ:" typ)  ; HACK
    (if-not (re-matches #"view/.*" typ)
      ; (do (println "  reducing...")  ; HACK
      (node :view/border
        :weight 1
        :margin 1
        :view/drawable/colors [ (node :view/rgb :red 0.2 :green 0.2 :blue 0.5) ]
        :item
        (node :view/section
          :items [
            (node :view/chars 
              :str typ
              :font :courier)
            (node :view/sequence
              :items [
                (node :view/quad)
                (node :view/section
                  :items
                  (vec 
                    (for [a (node-attrs n)]
                      (let [v (node-attr n a)]
                        (node :view/expr/flow
                          :boxes [
                            (node :view/expr/mono :str (subs (short-attr-name n a) 1))  ; TODO: strip node type
                            (node :view/expr/symbol :str :mapsto)
                            (reduceOne v)
                          ])))))
              ])
            ])))))
    
    