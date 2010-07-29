(ns meta.example.edit-view
  (:use (meta.edit draw expr nodes)
        (meta core reduce)
        (meta.clojure kernel))
  (:import (java.lang Integer)))

; (def p 
;   (ref 
;     (node :view/chars 
;       :str "hello"
;       :font :cmmi10)))

; (def p 
;   (ref 
;     (node :view/sequence
;       :items [
;       (node :view/chars 
;         :str "hello"
;         :font :cmmi10)
;       (node :view/chars 
;         :str ","
;         :font :cmr10)
;       (node :view/chars 
;         :str "world"
;         :font :cmmi10)
;       (node :view/chars 
;         :str "."
;         :font :cmr10)
;       (node :view/chars 
;         :str "poo"
;         :font :cmmi10)
;       ])))

(defn charSeq
  [from to font]
  (make-node :view/sequence [
      (node :view/chars
        :str (str "0x" (Integer/toHexString from))
        :font :cmr10)
      (node :view/quad)
      (make-node :view/sequence 
        (vec (interpose (node :view/thickspace)
                        (for [i (range from to)]
                          (node :view/chars
                            :str (str (char i))
                            :font :cmsy10)))))
      (node :view/quad)
      (node :view/chars
        :str (str "0x" (Integer/toHexString (dec to)))
        :font :cmr10)
    ]))


(def p 
  (make-node :view/section [
      (make-node :view/sequence [
        (node :view/chars
            :str "let"
            :font :cmbx10)
        (make-node :view/thickspace {})
        (node :view/chars
            :str "x"
            :font :cmmi10)
        (make-node :view/mediumspace {})
        (node :view/chars
            :str "="
            :font :cmr10)
        (make-node :view/mediumspace {})
        (node :view/chars
            :str "10"
            :font :cmr10)
        (make-node :view/thickspace {})
        (node :view/chars
            :str "in"
            :font :cmbx10)
        (make-node :view/thickspace {})
        (node :view/scripted
          :nucleus
          (make-node :view/sequence [
              (node :view/chars 
                :str "("
                :font :cmr10
                :view/drawable/color (node :view/gray :brightness 0.5))
              (node :view/chars 
                :str "x"
                :font :cmmi10)
              (node :view/thinspace)
              (node :view/chars 
                :str "+"
                :font :cmbx10)
              (node :view/thinspace)
              (node :view/chars 
                :str "2"
                :font :cmr10)
              (node :view/chars 
                :str ")"
                :font :cmr10
                :view/drawable/color (node :view/gray :brightness 0.5))
            ])
            
            :super
            (node :view/chars
              :str "2"
              :font :cmr10-script))
        ])
      
        (node :view/chars :str " " :font :cmr10)  ; HACK: need a vspace node?
        (node :view/chars :str "cmsy10 font table:" :font :times)
        (node :view/chars :str " " :font :cmr10)  ; HACK: need a vspace node?
        
        (charSeq 0x00 0x10 :cmsy10)
        (charSeq 0x10 0x20 :cmsy10)
        (charSeq 0x20 0x30 :cmsy10)
        (charSeq 0x30 0x40 :cmsy10)
        (charSeq 0x40 0x50 :cmsy10)
        (charSeq 0x50 0x60 :cmsy10)
        (charSeq 0x60 0x70 :cmsy10)
        (charSeq 0x70 0x80 :cmsy10)
        (charSeq 0x80 0x90 :cmsy10)
        (charSeq 0x90 0xa0 :cmsy10)
        (charSeq 0xa0 0xb0 :cmsy10)
        (charSeq 0xb0 0xc0 :cmsy10)
        (charSeq 0xc0 0xd0 :cmsy10)
        (charSeq 0xd0 0xe0 :cmsy10)
        (charSeq 0xe0 0xf0 :cmsy10)
        (charSeq 0xf0 0x100 :cmsy10)
      
        ; (node :view/chars
        ;   :str (apply str (map char (range 0x10 0x20)))
        ;   :font :cmsy10)
        ; (node :view/chars
        ;   :str (apply str (map char (range 0x20 0x30)))
        ;   :font :cmsy10)
        ; (node :view/chars
        ;   :str (apply str (map char (range 0x30 0x40)))
        ;   :font :cmsy10)
        ; (node :view/chars
        ;   :str (apply str (map char (range 0x40 0x50)))
        ;   :font :cmsy10)
        ; (node :view/chars
        ;   :str (apply str (map char (range 0x50 0x60)))
        ;   :font :cmsy10)
        ; (node :view/chars
        ;   :str (apply str (map char (range 0x60 0x70)))
        ;   :font :cmsy10)
        ; (node :view/chars
        ;   :str (apply str (map char (range 0x70 0x80)))
        ;   :font :cmsy10)
      ]))

; (print-node p)

(makeSyntaxFrame p "view/..." #(meta-reduce2 % (fn [n] nil)) {})
; (makeSyntaxFrame p "view/..." #(reduce-plus % (fn [n v] [nil v])) {})
