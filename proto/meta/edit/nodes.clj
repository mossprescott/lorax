; Functions over the low-level editor/presentation language, inspired by TeX's 
; horizontal list and math list concepts.

; TODO: grammar for the nodes
; TODO: optional alignment of certain elements to pixel (or half-pixel) boundaries?
; TODO: some kind of magnification factor?

(ns meta.edit.nodes
	(:use (meta core))
	(:import (java.awt 
	            BasicStroke
	            Color 
	            Font 
	            Graphics2D))
	(:import (java.awt.font 
	            FontRenderContext
	            GlyphVector))
	(:import (java.awt.geom 
	            Line2D$Float 
	            Rectangle2D
	            Rectangle2D$Float)))
	
(set! *warn-on-reflection* true)

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

; (def TEXT_SIZE 14)
; (def SCRIPT_SIZE 11)  ; Note: Knuth says 0.7x, which is more like 9.8
; (def SCRIPT_SCRIPT_SIZE 9)  ; Note: Knuth says 0.5x, which is 7

; (def TEXT_SIZE 12)
; (def SCRIPT_SIZE 10)  ; Note: Knuth says 0.7x, which is more like 9.8
; (def SCRIPT_SCRIPT_SIZE (* TEXT_SIZE 0.5))

; (def FONTS_OLD {
;   ;
;   ; TeX fonts, used for most purposes:
;   ;
;   
;   ; extended symbols
;   :cmex10 (Font. "jsMath-cmex10" Font/PLAIN TEXT_SIZE)
; 
;   ; math italics
;   :cmmi10 (Font. "jsMath-cmmi10" Font/PLAIN TEXT_SIZE)
;   :cmmi10-script (Font. "jsMath-cmmi10" Font/PLAIN SCRIPT_SIZE)
;   :cmmi10-scriptscript (Font. "jsMath-cmmi10" Font/PLAIN SCRIPT_SCRIPT_SIZE)
; 
;   ; text italics
;   :cmti10 (Font. "jsMath-cmti10" Font/PLAIN TEXT_SIZE)
;   :cmti10-script (Font. "jsMath-cmti10" Font/PLAIN SCRIPT_SIZE)
;   :cmti10-scriptscript (Font. "jsMath-cmti10" Font/PLAIN SCRIPT_SCRIPT_SIZE)
; 
;   ; math roman
;   :cmr10 (Font. "jsMath-cmr10" Font/PLAIN TEXT_SIZE)
;   :cmr10-script (Font. "jsMath-cmr10" Font/PLAIN SCRIPT_SIZE)
;   :cmr10-scriptscript (Font. "jsMath-cmr10" Font/PLAIN SCRIPT_SCRIPT_SIZE)
; 
;   ; bold extended (keywords)
;   :cmbx10 (Font. "jsMath-cmbx10" Font/PLAIN TEXT_SIZE)
;   :cmbx10-script (Font. "jsMath-cmbx10" Font/PLAIN SCRIPT_SIZE)
;   :cmbx10-scriptscript (Font. "jsMath-cmbx10" Font/PLAIN SCRIPT_SCRIPT_SIZE)
; 
;   ; bold extended (keywords)
;   :cmbx10-it (Font. "jsMath-cmbx10" Font/ITALIC TEXT_SIZE)
;   :cmbx10-it-script (Font. "jsMath-cmbx10" Font/ITALIC SCRIPT_SIZE)
;   :cmbx10-it-scriptscript (Font. "jsMath-cmbx10" Font/ITALIC SCRIPT_SCRIPT_SIZE)
; 
;   ; math symbol
;   ; For some reason, glyphs do not appear for any char < 0x2x,
;   ; although the jsMath site claims they are present.
;   :cmsy10 (Font. "jsMath-cmsy10" Font/PLAIN TEXT_SIZE)
;   :cmsy10-script (Font. "jsMath-cmsy10" Font/PLAIN SCRIPT_SIZE)
;   :cmsy10-scriptscript (Font. "jsMath-cmsy10" Font/PLAIN SCRIPT_SCRIPT_SIZE)
;   
;   ;
;   ; Other fonts, for special purposes:
;   ;
; 
;   :tiny (Font. "Lucida Grande" Font/PLAIN 9)  ; this is the most legible at small sizes?
;   
;   ; Times, a unicode font...
;   ; Unfortunately the spacing is quite different than the jsMath fonts
;   :times (Font. "Times New Roman" Font/PLAIN TEXT_SIZE)
;   :timesItalic (Font. "Times New Roman" Font/ITALIC TEXT_SIZE)
;   
;   ; used for monospace in a few places
;   :courier (Font. "Courier New" Font/PLAIN TEXT_SIZE)
;   :courierItalic (Font. "Courier New" Font/ITALIC TEXT_SIZE)
;   
;   :sans (Font. "Lucida Grande" Font/PLAIN 12);TEXT_SIZE)
;   :sans-script (Font. "Lucida Grande" Font/PLAIN 10);SCRIPT_SIZE)
;   :sans-scriptscript (Font. "Lucida Grande" Font/PLAIN 7);SCRIPT_SCRIPT_SIZE)
; })

; Global multiplier for the font size. 1.0 gets you something nice for 
; reading up close. 2.0 is probably better for across a room.
(def font-multiplier 1.0)

(def TEXT_SIZE_BASE 14)
(def FONT_SCALES {
  :text          1.0
  :script        0.8  ; truncates to 11
  :script-script 0.65 ; truncates to 9
})

(def FONTS {
  ;
  ; TeX fonts, used for most purposes:
  ;
  
  ; extended symbols
  :cmex10 ["jsMath-cmex10" Font/PLAIN :text]

  ; math italics
  :cmmi10 ["jsMath-cmmi10" Font/PLAIN :text]
  :cmmi10-script ["jsMath-cmmi10" Font/PLAIN :script]
  :cmmi10-scriptscript ["jsMath-cmmi10" Font/PLAIN :script-script]

  ; text italics
  :cmti10 ["jsMath-cmti10" Font/PLAIN :text]
  :cmti10-script ["jsMath-cmti10" Font/PLAIN :script]
  :cmti10-scriptscript ["jsMath-cmti10" Font/PLAIN :script-script]

  ; math roman
  :cmr10 ["jsMath-cmr10" Font/PLAIN :text]
  :cmr10-script ["jsMath-cmr10" Font/PLAIN :script]
  :cmr10-scriptscript ["jsMath-cmr10" Font/PLAIN :script-script]

  ; bold extended (keywords)
  :cmbx10 ["jsMath-cmbx10" Font/PLAIN :text]
  :cmbx10-script ["jsMath-cmbx10" Font/PLAIN :script]
  :cmbx10-scriptscript ["jsMath-cmbx10" Font/PLAIN :script-script]

  ; bold extended (keywords)
  :cmbx10-it ["jsMath-cmbx10" Font/ITALIC :text]
  :cmbx10-it-script ["jsMath-cmbx10" Font/ITALIC :script]
  :cmbx10-it-scriptscript ["jsMath-cmbx10" Font/ITALIC :script-script]

  ; math symbol
  ; For some reason, glyphs do not appear for any char < 0x2x,
  ; although the jsMath site claims they are present.
  :cmsy10 ["jsMath-cmsy10" Font/PLAIN :text]
  :cmsy10-script ["jsMath-cmsy10" Font/PLAIN :script]
  :cmsy10-scriptscript ["jsMath-cmsy10" Font/PLAIN :script-script]
  
  ;
  ; Other fonts, for special purposes:
  ;

  :tiny ["Lucida Grande" Font/PLAIN :script]  ; this is the most legible at small sizes?
  
  ; Times, a unicode font...
  ; Unfortunately the spacing is quite different than the jsMath fonts
  :times ["Times New Roman" Font/PLAIN :text]
  :timesItalic ["Times New Roman" Font/ITALIC :text]
  
  ; used for monospace in a few places
  :courier ["Courier New" Font/PLAIN :text]
  :courierItalic ["Courier New" Font/ITALIC :text]
  
  :sans ["Lucida Grande" Font/PLAIN :text] ; 12 would be better
  :sans-script ["Lucida Grande" Font/PLAIN :script] ; 10
  :sans-scriptscript ["Lucida Grande" Font/PLAIN :script-script] ; 7
})

(defn resolve-font-size 
  [scale]
  (int (* font-multiplier TEXT_SIZE_BASE scale)))
  
(defn resolve-font
  [kw]
  (let [ [name style mode] (FONTS kw) ]
    (Font. name style (resolve-font-size (FONT_SCALES mode)))))

;
; Color interpretation:
;
(defmulti color
  "Multi-method that takes :view/color node and returns an AWT Color instance."
  node-type)
  
(defmethod color :view/gray
  [n]
  (let [b (float (node-attr-value n :view/gray/brightness))]
    (Color. b b b)))  ; better way to get the right constructor?

(defmethod color :view/rgb
  [n]
  (Color. (float (node-attr-value n :view/rgb/red))
          (float (node-attr-value n :view/rgb/green))
          (float (node-attr-value n :view/rgb/blue))))

(defn node-color 
  "Takes a node which may or may not have a color attr, and returns an AWT Color 
  for drawing it."
  [n]
  (if (has-attr? n :view/drawable/color)
    (let [cn (node-attr n :view/drawable/color)]
      (color cn))
    DEFAULT_COLOR))


;
; Multi-methods for layout and drawing:
;

(defn- node-type-or-nil
  [n]
  (if (node? n) (node-type n)))

(defmulti size-impl
  "Multi-method calculating the size (in pixels) of a drawable.
  Takes [node g ctx] and returns a vector of floats [width height baseline].
  If the node should not be positioned relative to the baseline, it is nil."
  (fn [n gfx ctx] 
    ; (println "size-impl:" (node-id n))  ; HACK
    (node-type-or-nil n)))

(defmulti layout-impl
  "Returns a list of vectors [node x y w h] giving the bounds of each child
  (if any), relative to node's upper left corner."
  (fn [n gfx ctx] 
    ; (println "layout-impl:" (node-id n))  ; HACK
    (node-type-or-nil n)))

(defmulti draw-impl
  "Multi-method drawing just the visible portions of a node (that is, 
  not the children), assuming the upper-left corner is located at the origin."
  (fn [n gfx ctx debug?] (node-type-or-nil n)))


;
; Drawing context: memoizes size and layout calculations for efficiency
;
(defn make-draw-context
  []
  [(atom {}) (atom {})])

(defn size
  [n g ctx]
  (let [ [sizes layouts] ctx
         key (System/identityHashCode n)  ; Note: this will work even if ids are not unique
         ;_ (println "size-in-context:" id)  ; HACK
         ]
    (if-let [e (find @sizes key)]
      (val e)
      (let [ret (size-impl n g ctx)]
        (swap! sizes assoc key ret)
        ret))))

(defn layout
  [n g ctx]
  (let [ [sizes layouts] ctx
         key (System/identityHashCode n)  ; Note: this will work even if ids are not unique
         ; _ (println "layout-in-context:" id)  ; HACK
         ]
    (if-let [e (find @layouts key)]
      (val e)
      (let [ret (layout-impl n g ctx)]
        (swap! layouts assoc key ret)
        ret))))

(defn draw
  [n g ctx debug?]
  (draw-impl n g ctx debug?))

;
; String size calculation:
;

(defn memoize2
  "Returns a memoized version of a referentially transparent function, using 
  only the first two arguments as the key."
  [f]
  (let [mem (atom {})]
    (fn [a b & more]
      (let [key [a b]]
        (if-let [e (find @mem key)]
          (val e)
          (let [ret (apply f a b more)]
            (swap! mem assoc key ret)
            ret))))))

(def string-bounds
  ; Encapsulate size calculation for strings and memoize it to save the cost 
  ; repeated FontMetrics calculations. This becomes important only when various
  ; other bottlenecks are eliminated.
  ; This variant returns the size of the string's "bounding box," which is 
  ; tall enough for any possible ascender and/or descender, and wide anough for 
  ; the actual characters present. 
  (memoize2
    (fn [s f #^Graphics2D g]
      (let [fm (.getFontMetrics g (resolve-font f))]
        (.getStringBounds fm s g)))))

(def glyph-bounds
  ; Encapsulate size calculation for strings and memoize it to save the cost 
  ; repeated GlyphVector calculations. This becomes important only when various
  ; other bottlenecks are eliminated.
  ; This variant returns the "glyph pixel bounds," for the given characters, 
  ; which surrounds all the pixels that would be touched if the string were 
  ; rendered at position 0, 0. It would be more accurate to use the actual 
  ; (sub-pixel) drawing position, but then not referentially transparent, right?
  (memoize2
    (fn [^String s ^Font f ^Graphics2D g]
      (let [^FontRenderContext frc (.getFontRenderContext g)
            ^GlyphVector gv (.createGlyphVector f frc s)
            ^Rectangle2D rr (.getGlyphPixelBounds gv 0 frc 0 0)]
        rr))))

;
; Chars:
;

(defn- as-string
  "Tricky: a temporary workaround to avoid errors when the :str attr
  is a grammar/attr node, mostly."
  [str-attr]
  (cond 
    (value-node? str-attr) (str (node-value str-attr))
    (node? str-attr) (subs (str (node-type str-attr)) 1)  ; HACK
    true (str str-attr)))

(def BROKEN_DESCENDER_FONTS #{ :cmsy10 :cmsy10-script :cmsy10-scriptscript })

(defmethod size-impl :view/chars
  [n #^Graphics2D g ctx]
  (let [s (as-string (node-attr n :str))
        f (node-attr-value n :font)
        ; fm (.getFontMetrics g (resolve-font f))
        ; _ (println "str:" (node-id n) s)
        ; bounds (.getStringBounds fm s g)]
        #^Rectangle2D bounds (string-bounds s f g)]
      ; (println bounds)
      [(.getWidth bounds)
          ; Note: workaround weird descent in the symbol font for now
        (if (contains? BROKEN_DESCENDER_FONTS f)
          (- (.getMinY bounds))
          (.getHeight bounds))
        (- (.getMinY bounds))]))

(defmethod layout-impl :view/chars [n #^Graphics2D g ctx] [])

(defmethod draw-impl :view/chars
  [n #^Graphics2D g ctx debug?]
  (let [ [w h b] (size n g ctx)
          font (resolve-font (node-attr-value n :font))
          #^String s (as-string (node-attr n :str))
          ; _ (println "chars:" s) ; HACK
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
      (.drawString s (float 0) (float b)))))

;
; Space:
;
 ; TODO: scale with multiplier
; (def QUAD_WIDTH (* 1.0 TEXT_SIZE))  ; HACK: this is the quad width for cmmi

; TODO: reduce these spaces from expr, using different sizes for each mode
(defmethod size-impl :view/thinspace [n & more] 
  ; (let [ quadWidth QUAD_WIDTH ]
  ; [(* quadWidth (float 1/6)) 0 0]))  ; 3mu; see Knuth, p167
  [(resolve-font-size 0.15) 0 0]) ; truncates to 2
(defmethod layout-impl :view/thinspace [n & more] [])
(defmethod draw-impl :view/thinspace [n & more] nil)

(defmethod size-impl :view/mediumspace [n & more]
  ; (let [ quadWidth QUAD_WIDTH ]
  ; [(* quadWidth (float 2/9)) 0 0]))  ; 4mu; see Knuth, p167
  [(resolve-font-size 0.36) 0 0]) ; truncates to 5
(defmethod layout-impl :view/mediumspace [n & more] [])
(defmethod draw-impl :view/mediumspace [n & more] nil)

(defmethod size-impl :view/thickspace [n & more]
  ; (let [ quadWidth QUAD_WIDTH ]
  ; [(* quadWidth (float 5/18)) 0 0]))  ; 5mu; see Knuth, p167
  ; [8 0 0]))
  [(resolve-font-size 0.58) 0 0]) ; truncates to 8
(defmethod layout-impl :view/thickspace [n & more] [])
(defmethod draw-impl :view/thickspace [n & more] nil)

(defmethod size-impl :view/quad [n & more]
  ; TODO: calculate from the "current" font?
  ; [ QUAD_WIDTH 0 0 ])
  [(resolve-font-size 1.0) 0 0])
(defmethod layout-impl :view/quad [n & more] [])
(defmethod draw-impl :view/quad [n & more] nil)

;
; Scripted:
;

(defmethod size-impl :view/scripted
  [n #^Graphics2D g ctx]
  (let [nucl (node-attr n :nucleus)
        [_ _ nb] (size nucl g ctx)
        [[nucl nx ny nw nh] [sup sx sy sw sh]] (layout n g ctx)]
    [(+ sx sw) (+ ny nh) (+ ny nb)]))

(defmethod layout-impl :view/scripted 
  [n #^Graphics2D g ctx]
  (let [nucl (node-attr n :nucleus)
        sup (node-attr n :super)
        [nx ny nb] (size nucl g ctx)
        [sx sy sb] (size sup g ctx)]
    [ [nucl 0 (/ (if sb sb sy) 3) nx ny] [sup (+ 1 nx) 0 sx sy] ]))  ; HACK

(defmethod draw-impl :view/scripted [n & more])


;
; Sequence (horizontal list):
;

(defmethod size-impl :view/sequence
  ; height is the larger of max ascent + max descent, or max height
  [n #^Graphics2D g ctx]
  (let [items (node-children n)]
    (if (empty? items)
      [0 0 0]
      (let [szs (map #(size % g ctx) items)
            maxAsc (apply max 0 (for [ [w h b] szs :when b ] b))
            maxDesc (apply max 0 (for [ [w h b] szs :when b ] (- h b)))
            maxHeight (apply max 0 (for [ [w h b] szs ] h))
            maxAD (+ maxAsc maxDesc)
            w (reduce + 0 (for [ [w h b] szs ] w))
            h (max maxAD maxHeight)
            top (- (/ h 2) (/ maxAD 2))
            b (if (= maxAsc 0) nil (+ top maxAsc))]
        [ w h b ]))))

(defmethod layout-impl :view/sequence
  ; Align baselines, and vertically center any nodes without baselines.
  [n #^Graphics2D g ctx] 
  (let [ [w h b] (size n g ctx)]
    (loop [ items (node-children n) 
            x 0 
            result [] ]
        (if (empty? items)
          result
          (let [d (first items)
                ds (rest items)
                [dw dh db] (size d g ctx)
                y (if (and b db) 
                    (- b db)        ; use baseline
                    (/ (- h dh) 2)) ; center
                resultp (conj result [ d x y dw dh ])]
              (recur ds (+ x dw) resultp))))))

(defmethod draw-impl :view/sequence [n #^Graphics2D g ctx debug?] [])

;
; Section (vertical list):
;
(def LINE_SPACING 5)  ;; TODO: remove? make any spacing explicit?

(defmethod size-impl :view/section
  [ n #^Graphics2D g ctx]
  (let [items (node-children n)
        szs (map #(size % g ctx) items)]
    ; (println "n") (print-node n) ; HACK
    ; (println "szs" szs) ; HACK
    [ (apply max 0 (for [ [w h b] szs ] w))
      (- (reduce #(+ %1 (second %2) LINE_SPACING) 0 szs) LINE_SPACING)
      (if (seq szs) ((first szs) 2)) ])) ; use baseline of first child, if any

(defmethod layout-impl :view/section
  [n #^Graphics2D g ctx] 
  (let [ [w h b] (size n g ctx)]
    (loop [ items (node-children n) 
            y 0 
            result [] ]
        (if (empty? items)
          result
          (let [d (first items)
                ds (rest items)
                [dw dh db] (size d g ctx)
                resultp (conj result [ d 0 y dw dh ])]
      ;       ; (println "d ds" d ds)
              (recur ds (+ y dh LINE_SPACING) resultp))))))

(defmethod draw-impl :view/section [n #^Graphics2D g ctx debug?] [])


;
; Border:
;

(defmethod size-impl :view/border
  [n #^Graphics2D g ctx]
  (let [i (node-attr n :item)
        weight (node-attr-value n :weight)
        margin (node-attr-value n :margin)
        [w h b] (size i g ctx)]
    [ (+ weight margin w margin weight) 
      (+ weight margin h margin weight) 
      (if b 
        (+ weight margin b) 
        nil)]))
    
(defmethod layout-impl :view/border
  [n #^Graphics2D g ctx]
  (let [i (node-attr n :item)
        weight (node-attr-value n :weight)
        margin (node-attr-value n :margin)
        [w h b] (size i g ctx)]
    [ [i (+ weight margin) (+ weight margin) w h] ]))
    
(defmethod draw-impl :view/border
  [n #^Graphics2D g ctx debug?]
  (let [ [w h b] (size n g ctx) 
          weight (node-attr-value n :weight) 
          i (if (odd? weight) 0.5 0)  ; an adjustment to make it align to pixels usually
          x2 (+ i (int w))
          y2 (+ i (int h))
          colors (for [c (node-attr-children n :view/drawable/colors)] (color c))
          ; foo (println colors)
          ]
    (do
      (if (has-attr? n :fill)
        (doto g 
          (.setColor (color (node-attr n :fill)))
          (.fill (Rectangle2D$Float. i i (int w) (int h)))))
      (condp = (count colors)
        0
        nil
        
        1
        (doto g
          (.setStroke (BasicStroke. weight))
          (.setColor (nth colors 0))
          (.draw (Rectangle2D$Float. i i (int w) (int h))))
        
        2
        (doto g
          (.setStroke (BasicStroke. weight))
          (.setColor (nth colors 0))
          (.draw (Line2D$Float. i i x2 i))
          (.draw (Line2D$Float. i i i y2))
          (.setColor (nth colors 1))
          (.draw (Line2D$Float. x2 i x2 y2))
          (.draw (Line2D$Float. i y2 x2 y2)))))))


;
; Radical (currently square root only):
;

(def RADICALS [ "\u0070" "\u0071" "\u0072" "\u0073" ])  ; Note: \u0074 is the vertical one used to build really huge radicals

(def RADICAL_LINE_WEIGHT 1)
(def RADICAL_SPACE 1)  ; above and below the radicand

(defn- pick-glyph
  ; Picks the first str from the provided list which is at least as high as h,
  ; or else the last one.
  ; Returns [ str, ^Rectangle2D bounds ] for the chosen glyph.
  [h strs f ^Graphics2D g]
  (loop [strs (seq strs)]
    (let [s (first strs)
          ^Rectangle2D rr (glyph-bounds s f g)]
      (if (or (nil? (next strs))
              (>= (.getHeight rr) h))
        [s rr]
        (recur (next strs))))))

(defn- pick-radical
  [n g ctx]
  (let [x (node-attr n :radicand)
        [xw xh xb] (size x g ctx)]
    (pick-glyph (+ RADICAL_SPACE xh RADICAL_SPACE) RADICALS (resolve-font :cmex10) g)))

(defmethod size-impl :view/radical
  [n #^Graphics2D g ctx]
  (let [x (node-attr n :radicand)
        [xw xh xb] (size x g ctx)
        [rst ^Rectangle2D rr] (pick-radical n g ctx)
        rh (.getHeight rr)
        xy (max (+ RADICAL_LINE_WEIGHT RADICAL_SPACE) (/ (- rh xh) 2))]
    [ (+ xw (.getWidth rr)) 
      (max rh (+ RADICAL_LINE_WEIGHT RADICAL_SPACE xh RADICAL_SPACE ))
      (if xb (+ xy xb))]))
    
(defmethod layout-impl :view/radical
  [n #^Graphics2D g ctx]
  (let [x (node-attr n :radicand)
        [xw xh xb] (size x g ctx)
        [rst ^Rectangle2D rr] (pick-radical n g ctx)
        rh (.getHeight rr)
        xx (.getWidth rr)
        xy (max (+ RADICAL_LINE_WEIGHT RADICAL_SPACE) (/ (- rh xh) 2))]
    [ [x xx xy xw xh] ]))
    
(defmethod draw-impl :view/radical
  [n #^Graphics2D g ctx debug?]
  (let [x (node-attr n :radicand)
        [^String rst ^Rectangle2D rr] (pick-radical n g ctx)
        [[x xx xy xw xh]] (layout n g ctx)]
    (doto g
      (.setColor (node-color n))
      (.setFont (resolve-font :cmex10))
      (.drawString rst 0 0)
      (.setStroke (BasicStroke. 1))  ; Note: should take the weight from the font somehow
      (.draw (Line2D$Float. (+ 0.5 (int (.getWidth rr))) -0.5
                            (+ 0.5 (int (+ (.getWidth rr) xw))) -0.5)))))

;
; Delimiters:
;

(def DELIMS {
  ; All from cmex10 (Note: smaller sizes, with normal baselines, are in cmsy10)
  ; Note how randomly these are sprinkled around the font. JSMath's symbol 
  ; table has them in a much more rational order, so I must be missing something.
  "(" [ "\u00c0" "\u00b0" "\u00d2" "\u00ef" ]  ; growable: 30, 42, 31
  ")" [ "\u00c1" "\u00d1" "\u00d3" "\u0021" ]  ; growable: 40, 43, 41
  "[" [ "\u00c2" "\u0068" "\u00d4" "\u0022" ]  
  "]" [ "\u00c3" "\u0069" "\u00d5" "\u0023" ]
  :lfloor [ "\u00c4" "\u006a" "\u00d6" "\u0024" ] ; TODO... ; cmsy 0x62
  :rfloor [ "\u00c5" "\u006b" "\u00b8" "\u0025" ] ; cmsy 0x63
  :lceil [ "\u00c6" ] ; cmsy 0x64
  :rceil [ "\u00c7" ] ; cmsy 0x65
  "{" [ "\u00c8" "\u006e" "\u00da" "\u0028" ] ; cmsy 0x66
  "}" [ "\u00c9" "\u006f" "\u00db" "\u0029" ] ; cmsy 0x67
  :langle [ "\u00ca" ] ; cmsy 0x68
  :rangle [ "\u00cb" ] ; cmsy 0x69
  "|" [] ; cmsy 0x6a
  "||" []  ; cmsy 0x6b
  })

(def DELIM_SPACE 2)

(defmethod size-impl :view/delimited
  [n #^Graphics2D g ctx]
  (let [l (node-attr-value n :left)
        c (node-attr n :content)
        r (node-attr-value n :right)
        [cw ch cb] (size c g ctx)
        [lst ^Rectangle2D lr] (pick-glyph ch (DELIMS l) (resolve-font :cmex10) g)
        [rst ^Rectangle2D rr] (pick-glyph ch (DELIMS r) (resolve-font :cmex10) g)
        dh (max (.getHeight lr) (.getHeight rr))
        cy (/ (max (- dh ch) 0) 2)]
    [ (+ (.getWidth lr) DELIM_SPACE cw DELIM_SPACE (.getWidth rr)) 
      (max (.getHeight lr) ch (.getHeight rr))
      (if cb (+ cy cb))]))
    
(defmethod layout-impl :view/delimited
  [n #^Graphics2D g ctx]
  (let [l (node-attr-value n :left)
        c (node-attr n :content)
        r (node-attr-value n :right)
        [cw ch cb] (size c g ctx)
        [lst ^Rectangle2D lr] (pick-glyph ch (DELIMS l) (resolve-font :cmex10) g)
        [rst ^Rectangle2D rr] (pick-glyph ch (DELIMS r) (resolve-font :cmex10) g)
        dh (max (.getHeight lr) (.getHeight rr))
        cx (+ (.getWidth lr) DELIM_SPACE 1) ; not sure why I need this extra pixel, but it helps
        cy (/ (max (- dh ch) 0) 2)]
    [ [c cx cy cw ch] ]))
    
(defmethod draw-impl :view/delimited
  [n #^Graphics2D g ctx debug?]
  (let [l (node-attr-value n :left)
        c (node-attr n :content)
        r (node-attr-value n :right)
        [cw ch cb] (size c g ctx)
        [^String lst ^Rectangle2D lr] (pick-glyph ch (DELIMS l) (resolve-font :cmex10) g)
        [^String rst ^Rectangle2D rr] (pick-glyph ch (DELIMS r) (resolve-font :cmex10) g)
        [w h b] (size n g ctx)]
    (doto g
      (.setColor (node-color n))
      (.setFont (resolve-font :cmex10))
      (.drawString lst 
                   (float 0) 
                   (float (- (/ (- h (.getHeight lr)) 2) (.getMinY lr))))
      (.drawString rst 
                   (float (+ (.getWidth lr) DELIM_SPACE cw DELIM_SPACE))
                   (float (- (/ (- h (.getHeight rr)) 2) (.getMinY rr)))))))

;
; Over (i.e. fraction):
;

(def OVER_MARGIN 2)
(def OVER_SPACE 2)

(defmethod size-impl :view/over
  [n #^Graphics2D g ctx]
  (let [t (node-attr n :top)
        wt (node-attr-value n :weight)
        b (node-attr n :bottom)
        [tw th tb] (size t g ctx)
        [bw bh bb] (size b g ctx)]
    [ (+ OVER_MARGIN (max tw bw) OVER_MARGIN) 
      (+ th OVER_SPACE wt OVER_SPACE bh)
      nil]))
    
(defmethod layout-impl :view/over
  [n #^Graphics2D g ctx]
  (let [t (node-attr n :top)
        wt (node-attr-value n :weight)
        b (node-attr n :bottom)
        [tw th tb] (size t g ctx)
        [bw bh bb] (size b g ctx)]
    [ [t (+ OVER_MARGIN (/ (max 0 (- bw tw)) 2)) 0 tw th]
      [b (+ OVER_MARGIN (/ (max 0 (- tw bw)) 2)) (+ th OVER_SPACE wt OVER_SPACE) bw bh] ]))
    
(defmethod draw-impl :view/over
  [n #^Graphics2D g ctx debug?]
  (let [wt (node-attr-value n :weight)]
    (if (> wt 0)
      (let [t (node-attr n :top)
            b (node-attr n :bottom)
            [tw th tb] (size t g ctx)
            [bw bh bb] (size b g ctx)
            x1 0
            y1 (+ th OVER_SPACE -0.5)
            x2 (+ OVER_MARGIN (max tw bw) OVER_MARGIN)
            y2 y1]
        (doto g
          (.setColor (node-color n))
          (.setStroke (BasicStroke. wt))
          (.draw (Line2D$Float. x1 y1 x2 y2)))))))


;
; Handling of unrecognized nodes (or non-nodes), which allows the rest of the 
; program to be rendered.
;
(defmethod size-impl :default   [n & more] [0 0 nil])
(defmethod layout-impl :default [n & more] [])
(defmethod draw-impl :default   [n & more]
  (println "Unrecognized:" n))


;
; Last-resort reduction:
;

(defn- reduceOne
  "Reduce a single child of a node which was reduced by the generic reduction."
  [v]
  (condp = (node-type v) 
    :lorax/name
    (make-node :view/expr/keyword { :str (make-node :lorax/string (subs (str (node-value v)) 1)) })

    :lorax/string
    (do (print "v(string):" (node-value v) (value-node? v) " ") (print-node v)  ; HACK
    (make-node :view/chars { 
      :str (str \" (node-value v) \") 
      :font :cmr10 
      :view/drawable/color (make-node :view/rgb { :red 0 :green 0.7 :blue 0 })
    })
    ; (make-node :view/expr/string { :str (str \" (node-value v) \") })
    )  ; HACK
    
    :lorax/int
    (make-node :view/expr/int { :str (str (node-value v)) })
    
    v))
    ; (node? v) v
    ; 
    ; (vector? v) (make-node :view/section
    ;                 (vec (map reduceOne v)))
    ; 
    ; (string? v) (make-node :view/expr/string { :str v })
    ; 
    ; true (make-node :view/expr/keyword { :str (str v) })))


(defn reduceAny
  "Reduction which converts any non-view node, using a common presentation 
  which shows all children."
  [n]
  (let [typ (subs (str (node-type n)) 1)]
; (prn "typ:" typ)  ; HACK
    (cond
      ; TODO: use the reduction for any node with an error given only the :view grammar
      (not (or (re-matches #"view/.*" typ) (re-matches #"lorax/.*" typ)))
      ; (do (println "  reducing...")  ; HACK
      (node :view/border
        :weight 1
        :margin 1
        :view/drawable/colors [ (node :view/rgb :red 0.5 :green 0.5 :blue 0.5) ]
        :item
        (make-node :view/section [
            (make-node :view/chars {
              :str typ
              :font :courier
            })
            (make-node :view/sequence [
                (node :view/quad)
                (if (value-node? n)
                  (let [v (node-value n) _ (println "val:" v)]
                    (cond
                      (integer? v)
                      ; (make-node :view/expr/int { :str (str v) })
                      (make-node :view/chars {
                        :str (str v)
                        :font :cmr10
                      })
                      
                      (keyword? v)
                      (make-node :view/expr/keyword { :str (subs (str v) 1) })
                      
                      (string? v)
                      ; (make-node :view/expr/string { :str "abc" })
                      (do (println "str:" typ v)  ; HACK
                      (make-node :view/chars { 
                        :str (str \" v \") 
                        :font :cmr10 
                        :view/drawable/color (make-node :view/rgb { :red 0 :green 0.7 :blue 0 })
                      })
                      )  ; HACK
                      
                      true
                      ; (make-node :view/expr/mono { :str (str v) })))
                      (make-node :view/chars { 
                        :str (str v) 
                        :font :courier 
                        :view/drawable/color (make-node :view/rgb { :red 0.3 :green 0.3 :blue 0.3 })
                      })))
                  (make-node :view/section
                    (vec 
                      (for [a (node-attrs n)]
                        (let [v (node-attr n a)
                              ap (if (keyword? a) (subs (short-attr-name n a) 1) (str a))]
                          (make-node :view/sequence [
                              (make-node :view/expr/mono { :str ap })
                              (make-node :view/quad)
                              (make-node :view/expr/symbol { :str :mapsto })
                              (make-node :view/quad)
                              (reduceOne v)
                            ]))))))
              ])
            ])))))
    
    