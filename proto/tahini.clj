; stupid temperature converter app?

; Temp.: []
; Units: C or F
; <Convert>
; []

(unit
	(comp JFrame 
		:outlet frame  ; make the frame available externally
		:properties { :title "Convert Temperature" }
		:children [
			(comp JLabel "Temp:")
			(comp JTextField 
				:properties { :text (bind temp) })
			(comp JComboBox 
				:values [ "C" "F" ]
				:selected (bind temp-unit))
			(comp JButton
				:properties { :text "Convert" }
				:action { })
			(comp JTextArea
				:properties { :text 
					(let [convert ({ "C" toF, "F" toC }  temp-unit)
					 	t (convert temp)]
						(format "Temp: {0}" t)) })
		])

(defn toC [t] (* (- t 32) 5/9))
(defn toF [t] (+ (* t 5/9) 32))

; (tdef )
