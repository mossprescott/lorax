(ns meta.edit.select
  (:import 
    (java.awt 
      Color 
      Dimension 
      GridLayout 
      RenderingHints 
      Graphics2D 
      BasicStroke
      Transparency
      AlphaComposite
      GradientPaint
      GraphicsConfiguration
      Image)
    (java.awt.geom 
      Line2D 
      Line2D$Float
      Rectangle2D$Float
      RoundRectangle2D$Float)))

; TODO: why no HSV out of the box?!
(defn rgb [r g b] (Color. (float r) (float g) (float b)))

(def SELECTED_COLOR (rgb 1.0 0.7 0.85))
(def SELECTED_HILITE_COLOR (rgb 1.0 0.85 0.95))


(defn simple-hilite
  "Ugly, easy, and fast."
  [^Graphics2D g w h cs]
  (do
    (doto g
      (.setColor SELECTED_COLOR)
      (.setStroke (BasicStroke. 2))
      (.draw (Rectangle2D$Float. -2 -2 (+ w 4) (+ h 4)))
      (.setStroke (BasicStroke. 1)))
    ; (println "cs:" cs)
    (doseq [ [x y w h] cs ] 
      (.draw g (Rectangle2D$Float. (+ x -2) (+ y -2) (+ w 4) (+ h 4))))))


; TODO: hold on to the image between calls (with a var?)
(defn make-image
  [^Graphics2D g w h]
  (let [^GraphicsConfiguration gc (.getDeviceConfiguration g)]
    (.createCompatibleImage gc (max w 1) (max h 1) Transparency/TRANSLUCENT)))

(def OUTER_RADIUS_X 8)
(def OUTER_RADIUS_Y 6)
(def INNER_RADIUS_X 4)
(def INNER_RADIUS_Y 4)

(defn fancy-hilite
  [^Graphics2D g w h cs]
  (let [^Image img (make-image g (+ w 5) (+ h 5))
        ^Graphics2D g2 (.createGraphics img)
        x0 -2 y0 -2
        w0 (+ w 4) h0 (+ h 4)
        r (RoundRectangle2D$Float. x0 y0 w0 h0 OUTER_RADIUS_X OUTER_RADIUS_Y)
        hy (min (/ h 4) 100)]
    (doto g2
      (.translate (int (- x0)) (int (- y0)))
      
      ; clear the image (set alpha to 0):
      (.setComposite AlphaComposite/Clear)
      (.fillRect x0 y0 w0 h0)
      
      ; turn on anti-aliasing:
      (.setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
    
    
      ; For rendering the clip shape; color doesn't matter, this is just to set the alpha:
      
      ; First set the overall area:
      (.setComposite AlphaComposite/Src)
      (.fill r)
      
      ; Now clear the inner areas:
      (.setComposite AlphaComposite/Clear))
    (doseq [ [cx cy cw ch] cs ]
      (.fillRoundRect g2 cx cy cw ch INNER_RADIUS_X INNER_RADIUS_Y))
      
      ; Now set the outline again in case any of it was punched out:
    (doto g2
      (.setComposite AlphaComposite/Src)
      (.setStroke (BasicStroke. 2))
      (.draw r)
      
      ; Now the alpha is set, so we paint the whole rect, using the alpha as a 
      ; "clip". SrcAtop mode means use the color being painted, and the alpha 
      ; found in the image:
      (.setComposite AlphaComposite/SrcAtop)
      
      ; We draw two gradients, to get a slight 3-D look:
      (.setPaint (GradientPaint. 0 0 SELECTED_COLOR
                                 0 hy SELECTED_HILITE_COLOR))
      (.fillRect x0 y0 w0 (- hy y0))
      (.setPaint (GradientPaint. 0 hy SELECTED_HILITE_COLOR
                                 0 (* 2 hy) SELECTED_COLOR))
      (.fillRect x0 hy w0 (- h0 hy))
      
      ; And once around the edge to get a clean border (but still only painting 
      ; within the clip):
      (.setColor SELECTED_COLOR)
      (.setStroke (BasicStroke. 3))
      (.draw r)
      
      ; And with that, we're finished drawing to the image:
      (.dispose))
    
    (.drawImage g img x0 y0 nil)))
      

(defn draw-selection-hilite
  "Draw the graphics which indicates the selected node, at position 0, 0 in 
  the given graphics context.
  Params:
  w, h - size of the selected node
  cs - seq of bounds rects of children of the selected node"
  [g w h cs]
  ; (simple-hilite g w h cs))
  (fancy-hilite g w h cs))
