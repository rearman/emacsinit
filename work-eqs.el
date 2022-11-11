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
  "Calculate the CFM of a rectangular duct given fpm, width (in) and height (in)."
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
Returns a list of Sat. Water Press., Hum. Ratio, and Gns water/lb air."
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
	(let ((gns-water-lb-air (* hum-ratio 7000)))
	  (list sat-water-press hum-ratio gns-water-lb-air))))))

(defun k-to-c (tempk)
  "Convert degrees Kelvin to degrees Celsius."
  (- tempk 273.15))

(defun c-to-k (tempc)
  "Convert degrees Celsius to degrees Kelvin."
  (+ tempc 273.15))

(defun c-to-f (tempc)
  "Convert degrees Celsius to degrees Fahrenheit."
  (+ (* tempc 1.8) 32.0))

(defun f-to-c (tempf)
  "Convert degrees Fahrenheit to degrees Celsius."
  (/ (- tempf 32.0) 1.8))

(defun k-to-f (tempk)
  "Convert degrees Kelvin to degrees Fahrenheit.
Applies `c-to-f' to `k-to-c'."
  (c-to-f (k-to-c tempk)))

(defun f-to-k (tempf)
  "Convert degrees Fahrenheit to degrees Kelvin.
Applies `c-to-k' to `f-to-c'."
  (c-to-k (f-to-c tempf)))

(defun max-counts (resolution)
  "Calculate the max count for a PLC analog, given card's bit-resolution.
Formula is 1 - 2^(resolution)"
  (- (expt 2 resolution) 1))

(defun volts-to-counts (vin vmax resolution)
  "Convert a voltage signal to a PLC count.
Calculates a ratio of vin/vmax, then scales by `max-counts'."
  (* (/ (float vin) (float vmax)) (float (max-counts resolution))))

(defun counts-to-volts (cin vmax resolution)
  "Convert a PLC count to a voltage.
Calculates a ratio of cin/`max-counts', then multiplies by vmax."
  (* (/ cin (float (max-counts resolution))) vmax))
