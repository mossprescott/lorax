; Routines to export graphics as SVG (or PNG?) files.

(ns meta.edit.export
  ; (:use (clojure test)
  ;       (meta core edit reduce path)
  ;       (meta.clojure kernel)
  ;       (meta.edit nodes expr select))
  (:import 
    (java.awt Component)
    (org.apache.batik.svggen SVGGraphics2D)
    (org.apache.batik.dom GenericDOMImplementation)
    (org.apache.batik.apps.rasterizer 
      SVGConverter
      DestinationType)
    (org.w3c.dom Document
                 DOMImplementation)
    (java.io File
             FileOutputStream
             OutputStreamWriter
             Writer)))

(def USE_CSS true)

(defn render-to-svg
  "Render the given component to an SVG file in vector format. Note: images
  get written somehow or other, but presumably at screen res."
  [^Component comp ^String fname]
  (let [domImpl (GenericDOMImplementation/getDOMImplementation)
        svgNS "http://www.w3.org/2000/svg"
        document (.createDocument domImpl svgNS "svg" nil)
        ^SVGGraphics2D svgGenerator (SVGGraphics2D. document)]
    (.paint comp svgGenerator)
    (let [^Writer out (OutputStreamWriter. (FileOutputStream. fname)  "UTF-8")] ; HACK
      (.stream svgGenerator out (boolean USE_CSS))
      (println "Wrote SVG file: " fname))))

(defn render-to-pdf
  "Render the given component to a PDF file in vector format. Note: images
  get written somehow or other, but presumably at screen res."
  [^Component comp fname]
  (render-to-svg comp fname)
  (let [pdfName (str (subs fname 0 (- (count fname) 4)) ".pdf")]
    (doto (SVGConverter.)
        (.setSources (into-array [fname]))
        (.setDst (File. pdfName))
        (.setDestinationType DestinationType/PDF)
        (.setWidth (-> comp .getPreferredSize .getWidth))
        (.setHeight (-> comp .getPreferredSize .getHeight))
        (.execute))
    (println "Wrote PDF file:" pdfName)))

; TODO: PNG?

        ; // Get a DOMImplementation.
        ; DOMImplementation domImpl =
        ;     GenericDOMImplementation.getDOMImplementation();
        ; 
        ; // Create an instance of org.w3c.dom.Document.
        ; String svgNS = "http://www.w3.org/2000/svg";
        ; Document document = domImpl.createDocument(svgNS, "svg", null);
        ; 
        ; // Create an instance of the SVG Generator.
        ; SVGGraphics2D svgGenerator = new SVGGraphics2D(document);
        ; 
        ; // Ask the test to render into the SVG Graphics2D implementation.
        ; TestSVGGen test = new TestSVGGen();
        ; test.paint(svgGenerator);
        ; 
        ; // Finally, stream out SVG to the standard output using
        ; // UTF-8 encoding.
        ; boolean useCSS = true; // we want to use CSS style attributes
        ; Writer out = new OutputStreamWriter(System.out, "UTF-8");
        ; svgGenerator.stream(out, useCSS);
