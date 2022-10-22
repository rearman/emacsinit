;; WORK (NON-EDITING) RELATED DEFUNS
(defun square (x)
  "Calculate the square of a value."
  (* x x))

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
