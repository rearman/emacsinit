;;; init.el --- Quae configurare  -*- lexical-binding: t; -*-

;; PACKAGES
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)

(unless (equal system-type 'windows-nt)
  (use-package slime)
  (use-package ledger-mode))

(use-package visual-fill-column
  :config
  (global-visual-fill-column-mode))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 1)
  :config
  (global-corfu-mode)
  :bind
  ((:map corfu-map
	 ("C-h" . corfu-popupinfo-toggle))))

(add-hook 'eshell-mode-hook (lambda ()
			      (setq-local corfu-auto nil)
			      (corfu-mode)))

(use-package corfu-popupinfo
  :ensure nil ; Part of corfu
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(nil . 0.01))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

(use-package corfu-terminal
  :after corfu
  :unless (display-graphic-p)
  :config
  (corfu-terminal-mode))

(use-package orderless
  :custom ; Basic as fallback, overrides needed for tramp
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package expand-region
  :bind
  (("C-=" . er/expand-region)))

;; Put this here mainly for speed boost (only load magit when I call for it)
(use-package magit
  :init
  (message "LOCKED")
  :config
  (message "LOADED")
  :bind
  ((:map ctl-x-map
	 ("g" . magit-status))))

(use-package openwith
  :config
  (openwith-mode t))

(setq openwith-associations (list
			     (list (openwith-make-extension-regexp
				    '("xls" "xlsx" "doc" "docx"
				      "ppt" "odt" "ods" "odg" "odp"))
				   "LibreOffice"
				   '(file))
			     (list (openwith-make-extension-regexp
				    '("adpro"))
				   "ProductivitySuite"
				   '(file))))

(use-package org
  :ensure nil ; in emacs by default
  :custom
  (org-M-RET-may-split-line nil)
  (org-agenda-restore-windows-after-quit t)
  (org-use-fast-todo-selection 'expert)
  (org-src-window-setup 'current-window)
  (org-agenda-start-on-weekday nil)
  (org-catch-invisible-edits 'show)
  (org-directory "~/org")
  (org-agenda-files '("~/org/"
		      "~/org/kasten/"))
  (org-refile-targets '((nil :maxlevel . 9)
			(org-agenda-files :maxlevel . 9)))
  (org-default-notes-file (concat org-directory "/notes.org"))
  (safe-local-variable-values '((after-save-hook org-auto-archive)))
  (org-todo-keywords '((sequence "TODO(t@)" "WAITING(w@)" "IN-PROGRESS(i@)" "APPT(a@)" "|"
				 "DELEGATED(l@)" "DONE(d@)" "CANCELLED(c@)")))
  (org-capture-templates '(("n" "Note" entry (file+olp org-default-notes-file "Notes") "* %u %?")
			   ("t" "TODO" entry (file+olp org-default-notes-file "Tasks") "* TODO %? \n %u")
			   ("s" "Service" entry (file+olp org-default-notes-file "Service") "* TODO %? \n %u")
			   ("z" "Zettel" entry (file erfassen-zettel) "* %? ::")))
  (org-agenda-custom-commands '(("n" "Agenda and all TODOs"
				 ((agenda "") (alltodo "")))
				("u" "Unscheduled TODOs"
				 alltodo "" ((org-agenda-skip-function
					      (lambda nil
						(org-agenda-skip-entry-if 'scheduled
									  'deadline
									  'regexp "\n]+>")))
					     (org-agenda-overriding-header "Unscheduled TODO entries: ")))))
  :config
  (defun erfassen-zettel ()
    "Add a new zettel to the kasten.
Creates a new file <datestamp>.org in ~/org/kasten."
    (interactive)
    (expand-file-name (format "%s.org" (format-time-string "%Y-%m-%d-%H%M")) "~/org/kasten/"))

  (defun org-auto-archive ()
    "Automatically archive completed tasks in an org file.
Intended for use as an after-save-hook."
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
     "TODO=\"DONE\"|TODO=\"CANCELLED\"|TODO=\"DELEGATED\""
     'file)
    (save-buffer))
  :bind
  ((:map mode-specific-map
	 ("l" . org-store-link)
	 ("a" . org-agenda)
	 ("c" . org-capture))
   (:map org-mode-map
	 ("C-'" . nil))))

(setq-default indicate-empty-lines t
	      fill-column 79
	      cursor-type 'bar
	      cursor-in-non-selected-windows 'hollow)

(setq delete-old-versions t
      kept-new-versions 5
      kept-old-versions 5
      backup-by-copying t
      auto-save-default nil
      create-lockfiles nil
      backup-directory-alist `((".*" . ,temporary-file-directory)))

(setq require-final-newline t)

(setq ring-bell-function 'ignore
      use-short-answers t
      use-file-dialog nil
      read-buffer-completion-ignore-case t
      show-paren-delay 0
      show-paren-style 'expression
      display-time-default-load-average nil
      global-hl-line-sticky-flag t
      frame-title-format "Poor Man's LispM")

(setq history-delete-duplicates t
      save-interprogram-paste-before-kill t
      confirm-kill-processes nil
      dired-listing-switches "-alv --group-directories-first")

(setq prettify-symbols-alist '(("lambda" . 955)
			       ("delta" . 120517)
			       ("epsilon" . 120518)
			       ("->" . 8594)
			       ("<=" . 8804)
			       (">=" . 8805)))

(when (equal system-type 'windows-nt)
  (setq delete-by-moving-to-trash t
	ediff-diff-program "\"c:/Program Files/Git/usr/bin/diff.exe\""
	ediff-diff3-program "\"c:/Program Files/Git/usr/bin/diff3.exe\""
	diff-command "\"c:/Program Files/Git/usr/bin/diff.exe\""
	w32-recognize-altgr 'nil))

;; MODES
;; Looks
(global-hl-line-mode t)
(global-prettify-symbols-mode t)
(column-number-mode t)
(size-indication-mode t)
;; Close unused buffers
(midnight-mode t)
;; Make selection do what everyone expects
(delete-selection-mode t)
;; UI
(windmove-default-keybindings 'control) ; deprecate my C-L/R bindings
(context-menu-mode t) ; Right click bring up menu
(unless (display-graphic-p)
  (xterm-mouse-mode 1))

;; MATH DEFUNS
(defalias '^ 'expt)

(defun square (x)
  "Calculate the square of a value."
  (^ x 2))

(defun cube (x)
  "Calculate the cube of a value."
  (^ x 3))

(defun inv (x)
  "Calculate the inverse of a value."
  (/ 1 (float x)))

;; UNIT CONVERSION DEFUNS
(defun mm-to-in (mm)
  "Convert milimeters to inches."
  (/ mm 25.4))

(defun in-to-mm (in)
  "Convert inches to milimeters."
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
Applies `c-to-f' to `k-to-c'."
  (c-to-f (k-to-c tempk)))

(defun f-to-k (tempf)
  "Convert degrees Fahrenheit to degrees Kelvin.
Applies `c-to-k' to `f-to-c'."
  (c-to-k (f-to-c tempf)))

;; ELECTRICAL DEFUNS
(defun eff-imp (r1 &optional r2)
  "Calculate the effective input impedance, given two resistances. For use in `amps-to-volts' and related.  The handling of only one resistance given is done here, instead of doing it in every function that uses this."
  (if (eq nil r2)
      r1
    (inv (+ (inv (float r1)) (inv (float r2))))))

(defun amps-to-volts (amps r1 &optional r2)
  "Calculate the voltage, given amperage and impedance.
Multiplies the amperage by the effective impedance calculated with `eff-imp'."
  (* amps (eff-imp r1 r2)))

(defun volts-to-amps (volts r1 &optional r2)
  "Calculate the amperage, given voltage and impedance.
Divides the voltage by the effective impedance calculated with `eff-imp'."
  (/ volts (eff-imp r1 r2)))

;; PLC DEFUNS
(defun max-counts (resolution)
  "Calculate the max count for a PLC analog, given card's bit-resolution.
Formula is 2^(resolution) - 1"
  (- (^ 2 resolution) 1))

(defun volts-to-counts (vin vmax resolution)
  "Convert a voltage signal to a PLC count.
Calculates a ratio of vin/vmax, then scales by `max-counts'."
  (* (/ (float vin) (float vmax)) (float (max-counts resolution))))

(defun counts-to-volts (cin vmax resolution)
  "Convert a PLC count to a voltage.
Calculates a ratio of cin/`max-counts', then multiplies by vmax."
  (* (/ cin (float (max-counts resolution))) vmax))

(defun amps-to-counts (amps vmax resolution r1 &optional r2)
  "Convert a current signal to PLC Counts.
Converts the amperage to a voltage using `amps-to-volts' (with r1 and optional r2), then applies `volts-to-counts' to the resulting voltage, vmax, and resolution."
  (volts-to-counts (amps-to-volts amps r1 r2) vmax resolution))

(defun count-range (type upper lower vmax resolution &optional r1 r2)
  "Given an upper and lower signal, return the list of upper and lower PLC counts.
Type takes either a v or an i, corresponding to a voltage or current signal.  With a current signal, use r1 and maybe r2 to calculate `amps-to-counts'.  Otherwise ignore r1/r2 and calculate `volts-to-counts'."
  (cond ((or (equal 'v type)
	     (equal 'V type))
	 (list (volts-to-counts upper vmax resolution)
	       (volts-to-counts lower vmax resolution)))
	((or (equal 'i type)
	     (equal 'I type))
	 (list (amps-to-counts upper vmax resolution r1 r2)
	       (amps-to-counts lower vmax resolution r1 r2)))))

(defun scaleval (pmax pmin emax emin &optional ai)
  "Calculate the slope and offset given PLC Max/Min and Eng. Max/Min.
With optional argument `ai', also calculate a final scaled value from an input."
  (let* ((div (/ (- pmax pmin) (- (float emax) (float emin))))
	 (ofst (- emin (/ pmin div))))
    (if (eq nil ai)
	(list div ofst)
      (list div ofst (+ ofst (/ ai div))))))

(defun pid-tune (Kᵤ Tᵤ &optional loop-type)
  "Use the Ziegler-Nichols method to tune a P, PI or PID loop.
Takes Kᵤ, Tᵤ and optional loop-type (P, PI, or PID [default]) as arguments, and returns appropriate kp, ki, and kd."
  (cond ((or (equal 'p loop-type)
	     (equal 'P loop-type))
	 (list (* 0.5 Kᵤ)))
	((or (equal 'pi loop-type)
	     (equal 'PI loop-type))
	 (list (* 0.45 Kᵤ) (/ (* 0.54 Kᵤ) Tᵤ)))
	(t
	 (list (* 0.6 Kᵤ) (/ (* 1.2 Kᵤ) Tᵤ) (/ (* 3.0 Kᵤ Tᵤ) 40)))))

;; REFRIGERATION DEFUNS
(defun cfm-circ (fpm radius)
  "Calculate the CFM of a circular duct.
Inputs are feet/minute and radius (in)."
  (list (* pi fpm (square (/ radius 12)))))

(defun cfm-rect (fpm width height)
  "Calculate the CFM of a rectangular duct.
Inputs are feet/minute, width (in) and height (in)."
  (list (* fpm (/ width 12) (/ height 12))))

(defun gn-water-per-lb (temp humidity)
  "Calculate the grains of water per lb of air.
Inputs are temp (F) and humidity (%).  Returns a list of Saturated Water Pressure, Humidity Ratio, and Grains of water per lb of air."
  (let* ((sat-water-press (+ .0182795
			     (* temp .001029904)
			     (* (square temp) 0.00002579408)
			     (* (cube temp) (* 2.400493 (^ 10 -7)))
			     (* (^ temp 4) (* 8.100939 (^ 10 -10)))
			     (* (^ temp 5) (* 3.256805 (^ 10 -11)))
			     (* (^ temp 6) (* -1.001922 (^ 10 -13)))
			     (* (^ temp 7) (* 2.44161 (^ 10 -16)))))
	 (hum-press (* (/ humidity 100.0) sat-water-press))
	 (hum-ratio (/ (* hum-press 0.62198) (- 14.7 hum-press)))
	 (gns-water-lb-air (* hum-ratio 7000)))
    (list sat-water-press hum-ratio gns-water-lb-air)))

;; EDITING DEFUNS
(defun open-line-below (n)
  "Creates a new empty line below the current line and moves to it."
  (interactive "*p")
  (end-of-line)
  (open-line n)
  (next-line)
  (indent-for-tab-command))

(defun open-line-below-no-move (n)
  "Creates a new empty line below the current line, without moving point."
  (interactive "*p")
  (let ((pos (point-marker)))
    (end-of-line)
    (open-line n)
    (goto-char pos)))

(defun open-line-above (n)
  "Creates a new empty line above the current line."
  (interactive "*p")
  (beginning-of-line)
  (open-line n)
  (indent-for-tab-command))

(defun open-line-above-no-move (n)
  "Creates a new empty line above the current line, without moving point."
  (interactive "*p")
  (let ((pos (point-marker)))
    (beginning-of-line)
    (open-line n)
    (goto-char pos)))

(defun kill-bword-or-region ()
  "Kill region if active, otherwise kill back one word."
  (interactive)
  (if (region-active-p)
      (call-interactively 'kill-region)
    (call-interactively 'backward-kill-word)))

(defun backward-kill-line ()
  "Kill back to beginning of line from point."
  (interactive)
  (kill-line 0))

(defun backward-join-line ()
  "A wrapper for join-line to make it go in the right direction."
  (interactive)
  (join-line 0))

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.
If point was already at that position, move point to beginning of line. Stolen from BrettWitty's dotemacs github repo."
  (interactive "^")
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
	 (beginning-of-line))))

(defun go-to-scratch ()
  "Bring up the scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*")))

(defun scratch-only ()
  "Bring up the scratch buffer as the only visible buffer."
  (interactive)
  (go-to-scratch)
  (delete-other-windows))

(defun my-goto-line ()
  "Show line numbers before going to line, then hide them again."
  (interactive)
  (display-line-numbers-mode 1)
  (call-interactively 'goto-line)
  (display-line-numbers-mode -1))

;; ESHELL DEFUNS
(defun eshell-send-on-close-paren ()
  "Makes eshell act somewhat like genera.
Makes a closing paren execute the sexp.  Currently in test, look
out for errors."
  (interactive)
  (insert-char ?\))
  (let ((p (point)))
    ;;(eshell-bol)
    ;;(syntax-ppss-flush-cache (point))
    ;;(goto-char p)
    (cond ((= 0 (car (syntax-ppss)))
	   (eshell-send-input))
	  ((< (car (syntax-ppss)) 0)
	   (message "Check flush-cache"))
	  ((t nil)))))

;; *NIX DEFUNS
(unless (equal system-type 'windows-nt)
  (if(equal system-type 'berkeley-unix)
      (defun doas ()
	"Use TRAMP to reopen the current buffer as root using doas."
	(interactive)
	(when buffer-file-name
	  (find-alternate-file
	   (concat "/doas:root@localhost:"
		   buffer-file-name))))
    (defun sudo ()
      "Use TRAMP to reopen the current buffer as root using sudo."
      (interactive)
      (when buffer-file-name
	(find-alternate-file
	 (concat "/doas:root@localhost:"
		 buffer-file-name))))))

;; WINDOWS DEFUNS
(when (equal system-type 'windows-nt)
  (defun unfuck-aveva-license ()
    "Remove the offending xml files.
Use this when aveva can't find ass with both hands."
    (interactive)
    (let ((xml1 "c:/ProgramData/AVEVA/Licensing/License API2/Data/LocalAcquireInfo.xml")
	  (xml2 "c:/ProgramData/AVEVA/Licensing/License API2/Data/LocalBackEndAcquireInfo.xml"))
      (when (file-exists-p xml1) (delete-file xml1))
      (when (file-exists-p xml2) (delete-file xml2)))))

;; HOOKS
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(add-hook 'dired-mode-hook 'dired-hide-details-mode t)

(add-hook 'c-mode-common-hook (lambda ()
				(c-set-style "k&r")
				(setq tab-width 5
				      c-basic-offset 5
				      indent-tabs-mode t
				      comment-column 48)))

;; PUTS
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; FILE FINDER BINDINGS
(global-set-key (kbd "C-x M-f") 'find-file-at-point)
(global-set-key (kbd "C-x rB") 'bookmark-jump-other-window)

;; BUFFER BINDINGS
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-x M-b") 'buffer-menu-other-window)
(global-set-key (kbd "<f5>") 'go-to-scratch)
(global-set-key (kbd "S-<f5>") 'scratch-only)

;; MOTION BINDINGS
(global-set-key [remap move-beginning-of-line] 'smart-beginning-of-line)
(global-set-key [remap goto-line] 'my-goto-line)
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)

;; EDITING BINDINGS
(global-set-key (kbd "M-j")'backward-join-line)
(global-set-key (kbd "C-S-K") 'kill-whole-line)
(global-set-key (kbd "C-o")'open-line-below)
(global-set-key (kbd "M-o")'open-line-above)
(global-set-key (kbd "C-S-O")'open-line-below-no-move)
(global-set-key (kbd "M-S-O")'open-line-above-no-move)
(global-set-key (kbd "C-u")'backward-kill-line)
(global-set-key (kbd "C-w")'kill-bword-or-region)
(global-set-key (kbd "C-z") 'zap-up-to-char)
(global-set-key (kbd "C-\\") 'comment-or-uncomment-region)
(global-set-key (kbd "C-'") 'universal-argument) ; default C-u is overridden

;; SEARCH AND REPLACE BINDINGS
(global-set-key (kbd "M-s .") 'isearch-forward-thing-at-point)
(global-set-key (kbd "M-s M-.") 'isearch-forward-symbol-at-point)
(global-set-key (kbd "M-%") 'replace-regexp)
(global-set-key (kbd "C-%") 'replace-string)
(global-set-key (kbd "C-S-R") 'isearch-backward-regexp)
(global-set-key (kbd "C-S-S") 'isearch-forward-regexp)

;;  ESHELL BINDINGS
(global-set-key (kbd "C-c s") 'eshell)
(add-hook 'eshell-mode-hook (lambda () (define-key eshell-mode-map ")" 'eshell-send-on-close-paren)))

;; MISC BINDINGS
(global-set-key (kbd "M-C-z") 'eval-region)

;; CUSTOMIZE
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ledger-reports
   '(("assets" "%(binary) -f %(ledger-file) bal assets")
     ("income" "%(binary) -f %(ledger-file) bal income")
     ("expenses" "%(binary) -f %(ledger-file) bal expenses")
     ("liabilities" "%(binary) -f %(ledger-file) bal liabilities")
     ("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")))
 '(minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
 '(package-selected-packages
   '(visual-fill-column corfu-popupinfo corfu-terminal orderless corfu magit browse-kill-ring use-package openwith expand-region)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:background "grey75" :foreground "black"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey90" :foreground "grey20" :weight light))))
 '(show-paren-match ((t (:underline t)))))
