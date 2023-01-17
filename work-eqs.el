;; WORK (NON-EDITING) RELATED DEFUNS
(defun square (x)
  "Calculate the square of a value."
  (expt x 2))

(defun cube (x)
  "Calculate the cube of a value."
  (expt x 3))

(defun inv (x)
  "Calculate the inverse of a value."
  (/ 1 x))

(defun scaleval (pmax pmin emax emin &optional ai)
  "Calculate the slope and offset given PLC Max/Min and Eng. Max/Min.
With optional argument 'ai', also calculate a final scaled value from an input."
  (let ((div (/ (- pmax pmin) (- (float emax) (float emin)))))
    (let ((ofst (- emin (/ pmin div))))
      (if (eq nil ai)
	  (list div ofst)
	(let ((final (+ ofst (/ ai div))))
	  (list div ofst final))))))

(defun cfm-circ (fpm radius)
  "Calculate the CFM of a circular duct given fpm and radius (in)."
  (list (* pi fpm (square (/ radius 12)))))

(defun cfm-rect (fpm width height)
  "Calculate the CFM of a rectangular duct given fpm, width (in) and height (in)."
  (list (* fpm (/ width 12) (/ height 12))))

(defun pid-tune (Ku Tu &optional p-type)
  "Use the Ziegler-Nichols method to tune a P, PI or PID loop.
Takes Ku, Tu and optional p-type (P, PI, or PID [default]) as arguments,
 and returns appropriate kp, ki, and kd."
  (cond ((or (equal 'p p-type)
	     (equal 'P p-type))
	 (list (* 0.5 Ku)))
	((or (equal 'pi p-type)
	     (equal 'PI p-type))
	 (list (* 0.45 Ku) (/ (* 0.54 Ku) Tu)))
	(t (list (* 0.6 Ku) (/ (* 1.2 Ku) Tu) (/ (* 3.0 Ku Tu) 40)))))

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

(defun mm-to-in (mm)
  "Convert milimeters to inches."
  (/ mm 25.4))

(defun in-to-mm (in)
  "Convert milimeters to inches."
  (* in 25.4))

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
Applies 'c-to-f' to 'k-to-c'."
  (c-to-f (k-to-c tempk)))

(defun f-to-k (tempf)
  "Convert degrees Fahrenheit to degrees Kelvin.
Applies 'c-to-k' to 'f-to-c'."
  (c-to-k (f-to-c tempf)))

(defun max-counts (resolution)
  "Calculate the max count for a PLC analog, given card's bit-resolution.
Formula is 1 - 2^(resolution)"
  (- (expt 2 resolution) 1))

(defun volts-to-counts (vin vmax resolution)
  "Convert a voltage signal to a PLC count.
Calculates a ratio of vin/vmax, then scales by 'max-counts'."
  (* (/ (float vin) (float vmax)) (float (max-counts resolution))))

(defun counts-to-volts (cin vmax resolution)
  "Convert a PLC count to a voltage.
Calculates a ratio of cin/'max-counts', then multiplies by vmax."
  (* (/ cin (float (max-counts resolution))) vmax))

(defun eff-imp (r1 &optional r2)
  "Calculate the effective input impedance, given two resistances.
For use in 'amps-to-volts' and related.  The handling of only one resistance
given is done here, instead of doing it in every function that uses this."
  (if (eq nil r2)
      r1
    (inv (+ (inv (float r1)) (inv (float r2))))))

(defun amps-to-volts (amps r1 &optional r2)
  "Calculate the voltage, given amperage and impedance.
Multiplies the amperage by the effective impedance calculated with 'eff-imp'."
  (* amps (eff-imp r1 r2)))

(defun volts-to-amps (volts r1 &optional r2)
  "Calculate the amperage, given voltage and impedance.
Divides the voltage by the effective impedance calculated with 'eff-imp'."
  (/ volts (eff-imp r1 r2)))

(defun amps-to-counts (amps vmax resolution r1 &optional r2)
  "Convert an amp signal to PLC Counts.
Converts the amperage to a voltage using 'amps-to-volts' (with r1 and optional r2),
then applies 'volts-to-counts' to the resulting voltage, vmax, and resolution."
  (volts-to-counts (amps-to-volts amps r1 r2) vmax resolution))

(defun count-range (type upper lower vmax resolution &optional r1 r2)
  "Given an upper and lower signal, return the list of upper and lower PLC counts.
Type takes either a v or an i, corresponding to a voltage or current signal.
With a current signal, use r1 and maybe r2 to calculate 'amps-to-counts'.
Otherwise ignore r1/r2 and calculate 'volts-to-counts'."
  (cond ((or (equal 'v type)
	     (equal 'V type))
	 (list (volts-to-counts upper vmax resolution)
	       (volts-to-counts lower vmax resolution)))
	((or (equal 'i type)
	     (equal 'I type))
	 (list (amps-to-counts upper vmax resolution r1 r2)
	       (amps-to-counts lower vmax resolution r1 r2)))))
