;; WORK (NON-EDITING) RELATED DEFUNS
(defun square (x)
  "Calculate the square of a value."
  (expt x 2))

(defun cube (x)
  "Calculate the cube of a value."
  (expt x 3))

(defun scaleval (pmax pmin emax emin &optional ai)
  "Calculate the slope and offset given PLC Max/Min and Eng. Max/Min.
With optional argument 'ai', also calculate a final scaled value from an input."
  (let ((div (/ (- pmax pmin) (- emax emin))))
    (let ((ofst (- emin (/ pmin div))))
      (if (not (eq nil ai))
	  (let ((final (+ ofst (/ ai div))))
	    (list div ofst final))
	(list div ofst)))))

(defun cfm-circ (fpm radius)
  "Calculate the CFM of a circular duct given fpm and radius (in)."
  (list (* pi fpm (square (/ radius 12)))))

(defun cfm-rect (fpm width height)
  "Calculate the CFM of a circular duct given fpm, width (in) and height (in)."
  (list (* fpm (/ width 12) (/ height 12))))

(defun pid-tune (Ku Tu p-type)
  "Use the Ziegler-Nichols method to tune a P, PI or PID loop.
Takes Ku, Tu and p-type (P, PI, or PID) as arguments, and returns appropriate kp, ki, and kd."
  (cond ((or (equal 'p p-type)
	     (equal 'P p-type))
	 (list (* 0.5 Ku)))
	((or (equal 'pi p-type)
	     (equal 'PI p-type))
	 (list (* 0.45 Ku) (/ (* 0.54 Ku) Tu)))
	((or (equal 'pid p-type)
	     (equal 'PID p-type))
	 (list (* 0.6 Ku) (/ (* 1.2 Ku) Tu) (/ (* 3.0 Ku Tu) 40)))))

(defun gn-water-per-lb (temp humidity)
  "Calculate the grains of water per lb of air, given temperature (F) and humidity (%).
Returns a list of Sat. Water Press., Hum. Ratio, and Gns Moisture/lb air."
  (let ((sat-water-press (+ .0182795
			    (* temp .001029904)
			    (* (square temp) 0.00002579408)
			    (* (cube temp) (* 2.400493 (expt 10 -7)))
			    (* (expt temp 4) (* 8.100939 (expt 10 -10)))
			    (* (expt temp 5) (* 3.256805 (expt 10 -11)))
			    (* (expt temp 6) (* -1.001922 (expt 10 -13)))
			    (* (expt temp 7) (* 2.44161 (expt 10 -16))))))
    (let ((hum-press (* (/ humidity 100.0) sat-water-press)))
      (let ((hum-ratio (/ (* hum-press 0.62198) (- 14.7 hum-press))))
	(let ((gns-mstr-lb-air (* hum-ratio 7000)))
	  (list sat-water-press hum-ratio gns-mstr-lb-air))))))
