;;; init.el --- Quae configurare  -*- lexical-binding: t; -*-

;; Haec fasciculus pars Emacs non est.

;;; Code:

;; PACKAGES
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(unless (eq system-type 'windows-nt)
  (use-package slime)
  (use-package ledger-mode))

(use-package company
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 1)
  (company-require-match nil)
  (company-show-quick-access t)
  (company-format-margin-function 'company-text-icons-margin)
  (company-text-face-extra-attributes '(:slant italic))
  (company-selection-wrap-around t)
  (company-tooltip-flip-when-above t)
  (company-insertion-on-trigger nil)
  (company-dabbrev-other-buffers t)
  (company-dabbrev-code-other-buffers t)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case 'keep-prefix)
  :config
  (global-company-mode t))

(setq company-tooltip-idle-delay 10
      company-frontends '(company-pseudo-tooltip-unless-just-one-frontend-with-delay
			   company-preview-frontend
			   company-echo-metadata-frontend))
(define-key company-active-map [tab] (lambda ()
					    (interactive)
					    (let ((company-tooltip-idle-delay 0.0))
					      (company-complete)
					      (and company-candidates
						   (company-call-frontends 'post-command)))))

(use-package expand-region
  :bind
  (("C-=" . er/expand-region)))

(use-package git-commit
  :custom
  (git-commit-fill-column 72)
  (git-commit-style-convention-checks '(non-empty-second-line overlong-summary-line)))

;; Put this here mainly for speed boost (only load magit when I call for it)
(use-package magit
  :init
  (message "Loading Magit...")
  :config
  (message "Loaded Magit!")
  :bind
  ((:map ctl-x-map
	 ("g" . magit-status))))

(use-package openwith
  :config
  (openwith-mode t))

(if (eq system-type 'windows-nt)
    (setq delete-by-moving-to-trash t
	  ediff-diff-program "\"c:/Program Files/Git/usr/bin/diff.exe\""
	  ediff-diff3-program "\"c:/Program Files/Git/usr/bin/diff3.exe\""
	  diff-command "\"c:/Program Files/Git/usr/bin/diff.exe\""
	  openwith-associations '(("\\.pdf\\'" "sumatrapdf" (file))
				  ("\\.xls\\'" "excel" (file))
				  ("\\.xlsx\\'" "excel" (file))
				  ("\\.doc\\'" "word" (file))
				  ("\\.docx\\'" "word" (file))
				  ("\\.adpro\\'" "PoductivitySuite" (file))))
  (setq openwith-associations '(("\\.pdf\\'" "zathura" (file))
				("\\.xls\\'" "libreoffice" (file))
				("\\.xlsx\\'" "libreoffice" (file))
				("\\.doc\\'" "libreoffice" (file))
				("\\.docx\\'" "libreoffice" (file)))))

(use-package org
  :ensure nil ; in emacs by default
  :custom
  (org-M-RET-may-split-line nil)
  (org-reverse-note-order t)
  (org-agenda-window-setup 'other-window)
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
  (org-todo-keywords '((sequence "TODO(t@)"
				 "WAITING(w@)"
				 "IN-PROGRESS(i@)"
				 "APPT(a@)"
				 "|"
				 "DELEGATED(l@)"
				 "DONE(d@)"
				 "CANCELLED(c@)")))
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

(use-package page-break-lines
  :custom
  (page-break-lines-max-width (- (window-width) 1))
  :config
  (global-page-break-lines-mode))

(use-package browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings))

(setq-default indicate-empty-lines t
	      fill-column 80
	      read-file-name-completion-ignore-case t
	      read-buffer-completion-ignore-case t
	      cursor-type 'bar
	      cursor-in-non-selected-windows 'hollow)

(setq require-final-newline t
      text-quoting-style 'straight
      ring-bell-function 'ignore
      use-short-answers t
      mode-line-compact t
      backup-directory-alist '(("." . "~/emacs-backups"))
      version-control t
      delete-old-versions t
      auto-save-default nil
      confirm-nonexistent-file-or-buffer nil
      show-paren-delay 0
      show-paren-style 'expression
      blink-matching-paren 'jump
      global-hl-line-sticky-flag t
      echo-keystrokes 0.01
      history-delete-duplicates t
      save-interprogram-paste-before-kill t
      confirm-kill-processes nil
      auto-window-vscroll nil
      frame-title-format "Poor Man's LispM"
      eshell-destroy-buffer-when-process-dies t
      dired-listing-switches "-alv --group-directories-first"
      ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function (if (> (frame-width) 150)
				      'split-window-horizontally
				    'split-window-vertically)
      prettify-symbols-alist '(("lambda" . 955)
			       ("delta" . 120517)
			       ("epsilon" . 120518)
			       ("->" . 8594)
			       ("<=" . 8804)
			       (">=" . 8805)))

;; MODES
;;(fringe-mode 0)
(global-hl-line-mode t)
(global-prettify-symbols-mode t)
(column-number-mode t)
(size-indication-mode t)

(midnight-mode t)
(save-place-mode 1)

(global-visual-line-mode t)
(delete-selection-mode t)
(unless (display-graphic-p)
  (xterm-mouse-mode 1))
;; UTF8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)

;; MATH DEFUNS
(defun square (x)
  "Calculate the square of a value."
  (expt x 2))

(defun cube (x)
  "Calculate the cube of a value."
  (expt x 3))

(defun inv (x)
  "Calculate the inverse of a value."
  (/ 1 (float x)))

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
Applies 'c-to-f' to 'k-to-c'."
  (c-to-f (k-to-c tempk)))

(defun f-to-k (tempf)
  "Convert degrees Fahrenheit to degrees Kelvin.
Applies 'c-to-k' to 'f-to-c'."
  (c-to-k (f-to-c tempf)))

;; ELECTRICAL DEFUNS
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

;; PLC DEFUNS
(defun max-counts (resolution)
  "Calculate the max count for a PLC analog, given card's bit-resolution.
Formula is 2^(resolution) - 1"
  (- (expt 2 resolution) 1))

(defun volts-to-counts (vin vmax resolution)
  "Convert a voltage signal to a PLC count.
Calculates a ratio of vin/vmax, then scales by 'max-counts'."
  (* (/ (float vin) (float vmax)) (float (max-counts resolution))))

(defun counts-to-volts (cin vmax resolution)
  "Convert a PLC count to a voltage.
Calculates a ratio of cin/'max-counts', then multiplies by vmax."
  (* (/ cin (float (max-counts resolution))) vmax))

(defun amps-to-counts (amps vmax resolution r1 &optional r2)
  "Convert a current signal to PLC Counts.
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

(defun scaleval (pmax pmin emax emin &optional ai)
  "Calculate the slope and offset given PLC Max/Min and Eng. Max/Min.
With optional argument 'ai', also calculate a final scaled value from an input."
  (let* ((div (/ (- pmax pmin) (- (float emax) (float emin))))
	 (ofst (- emin (/ pmin div))))
    (if (eq nil ai)
	(list div ofst)
      (list div ofst (+ ofst (/ ai div))))))

(defun pid-tune (Kᵤ Tᵤ &optional loop-type)
  "Use the Ziegler-Nichols method to tune a P, PI or PID loop.
Takes Kᵤ, Tᵤ and optional loop-type (P, PI, or PID [default]) as arguments,
 and returns appropriate kp, ki, and kd."
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
  "Calculate the CFM of a circular duct given feet/minute and radius (in)."
  (list (* pi fpm (square (/ radius 12)))))

(defun cfm-rect (fpm width height)
  "Calculate the CFM of a rectangular duct given feet/minute, width (in) and height (in)."
  (list (* fpm (/ width 12) (/ height 12))))

(defun gn-water-per-lb (temp humidity)
  "Calculate the grains of water per lb of air, given temp (F) and
humidity (%).
Returns a list of Saturated Water Pressure, Humidity Ratio, and Grains of water per lb of air."
  (let* ((sat-water-press (+ .0182795
			     (* temp .001029904)
			     (* (square temp) 0.00002579408)
			     (* (cube temp) (* 2.400493 (expt 10 -7)))
			     (* (expt temp 4) (* 8.100939 (expt 10 -10)))
			     (* (expt temp 5) (* 3.256805 (expt 10 -11)))
			     (* (expt temp 6) (* -1.001922 (expt 10 -13)))
			     (* (expt temp 7) (* 2.44161 (expt 10 -16)))))
	 (hum-press (* (/ humidity 100.0) sat-water-press))
	 (hum-ratio (/ (* hum-press 0.62198) (- 14.7 hum-press)))
	 (gns-water-lb-air (* hum-ratio 7000)))
    (list sat-water-press hum-ratio gns-water-lb-air)))

;; EDITING DEFUNS
(defun open-line-below ()
  "Creates a new empty line below the current line."
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  "Creates a new empty line above the current line.
Can't go prev line first, edge case of beginning of buffer."
  (interactive)
  (beginning-of-line)
  (newline)
  (previous-line)
  (indent-for-tab-command))

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
If point was already at that position, move point to beginning of line.
Stolen from BrettWitty's dotemacs github repo."
  (interactive "^")
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
	 (beginning-of-line))))

(defun slot/get-queries (&optional pairs)
  "Get multiple 'query-replace' pairs from the user.
PAIRS is a list of replacement pairs of the form (FROM . TO).
Stolen from https://tony-zorman.com/posts/query-replace/2022-08-06-query-replace-many.html"
  (-let* (((from to delim arg)
	   (query-replace-read-args
	    (s-join " "
		    (-non-nil
		     (list "Replace Strings"
			   (cond ((eq current-prefix-arg '-) "backward")
				 (current-prefix-arg         "word"))
			   (when (use-region-p) "in region"))))
	    nil))                       ; no regexp-flag
	  (from-to (cons (regexp-quote from)
			 (s-replace "\\" "\\\\" to))))
	 ;; HACK: Since the default suggestion of replace.el will be
	 ;; the last one we've entered, an empty string will give us
	 ;; exactly that.  Instead of trying to fight against this,
	 ;; use it in order to signal an exit.
	 (if (-contains? pairs from-to)
	     (list pairs delim arg)
	   (slot/get-queries (push from-to pairs)))))

(defun slot/replace-string-many
    (pairs &optional delimited start end backward region-noncontiguous-p)
  "Like 'replace-string', but query for several replacements.
Query for replacement pairs until the users enters an empty
string (but see 'slot/get-queries').

Refer to 'query-replace' and 'perform-replace' for what the other
arguments actually mean.
Stolen from https://tony-zorman.com/posts/query-replace/2022-08-06-query-replace-many.html
Edited and renamed to remove the Query."
  (interactive
   (let ((common (slot/get-queries)))
     (list (nth 0 common) (nth 1 common)
	   (if (use-region-p) (region-beginning))
	   (if (use-region-p) (region-end))
	   (nth 2 common)
	   (if (use-region-p) (region-noncontiguous-p)))))
  (perform-replace
   (concat "\\(?:" (mapconcat #'car pairs "\\|") "\\)") ; build query
   (cons (lambda (pairs _count)
	   (cl-loop for (from . to) in pairs
		    when (string-match from (match-string 0))
		    return to))
	 pairs)
   nil :regexp
   delimited nil nil start end backward region-noncontiguous-p))

(defun slot/query-replace-many
    (pairs &optional delimited start end backward region-noncontiguous-p)
  "Like 'query-replace', but query for several replacements.
Query for replacement pairs until the users enters an empty
string (but see 'slot/get-queries').

Refer to 'query-replace' and 'perform-replace' for what the other
arguments actually mean.
Stolen from https://tony-zorman.com/posts/query-replace/2022-08-06-query-replace-many.html"
  (interactive
   (let ((common (slot/get-queries)))
     (list (nth 0 common) (nth 1 common)
	   (when (use-region-p) (region-beginning))
	   (when (use-region-p) (region-end))
	   (nth 2 common)
	   (when (use-region-p) (region-noncontiguous-p)))))
  (perform-replace
   (concat "\\(?:" (mapconcat #'car pairs "\\|") "\\)") ; build query
   (cons (lambda (pairs _count)
	   (cl-loop for (from . to) in pairs
		    when (string-match from (match-string 0))
		    return to))
	 pairs)
   :query :regexp
   delimited nil nil start end backward region-noncontiguous-p))

(defun scratch-only ()
  "Bring up the scratch buffer as the only visible buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
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
Makes a closing paren execute the sexp.  Currently in test, look out for errors."
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
    "Remove the offending xml files when aveva can't find ass with both hands."
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
(define-key mode-specific-map "ee" (lambda () (interactive) (dired user-emacs-directory)))
(define-key mode-specific-map "ei" (lambda () (interactive) (find-file user-init-file)))
(define-key global-map [f8] 'recentf-open-files)
(define-key ctl-x-map "r\S-B" 'bookmark-jump-other-window)

;; BUFFER BINDINGS
(define-key ctl-x-map "\C-b" 'buffer-menu)
(define-key mode-specific-map "b" 'buffer-menu-other-window)
(define-key global-map [C-left] 'previous-window-any-frame)
(define-key global-map [C-right] 'next-window-any-frame)
(define-key esc-map [C-left] 'previous-buffer) ; also bound to C-x <left> by default
(define-key esc-map [C-right] 'next-buffer) ; also bound to C-x <right> by default
(define-key global-map [f5] 'scratch-only)

;; MOTION BINDINGS
(define-key global-map [home] 'beginning-of-buffer)
(define-key global-map [end] 'end-of-buffer)
(define-key global-map [remap move-beginning-of-line] 'smart-beginning-of-line)
(define-key global-map [remap goto-line] 'my-goto-line)

;; EDITING BINDINGS
(define-key global-map "\C-o" 'open-line-below)
(define-key global-map "\M-j" 'backward-join-line)
(define-key global-map "\M-o" 'open-line-above)
(define-key global-map "\C-u" 'backward-kill-line)
(define-key global-map "\C-w" 'kill-bword-or-region)
(define-key global-map "\C-z" 'zap-up-to-char)
(define-key mode-specific-map ";" 'comment-or-uncomment-region)
(define-key global-map [?\C-\S-K] 'kill-whole-line)
(define-key global-map [?\C-'] 'universal-argument) ; default is C-u

;; SEARCH AND REPLACE BINDINGS
(define-key esc-map "s." 'isearch-forward-thing-at-point)
(define-key esc-map "s\M-." 'isearch-forward-symbol-at-point)
(define-key esc-map "%" 'slot/replace-string-many)
(define-key esc-map [?\C-%] 'slot/query-replace-many)
(define-key global-map [?\C-%] 'replace-regexp)
(define-key global-map [?\C-\S-R] 'isearch-backward-regexp)
(define-key global-map [?\C-\S-S] 'isearch-forward-regexp)

;;  ESHELL BINDINGS
(define-key mode-specific-map "s" 'eshell)
(add-hook 'eshell-mode-hook (lambda () (define-key eshell-mode-map ")" 'eshell-send-on-close-paren)))

;; MISC BINDINGS
(define-key esc-map "\C-z" 'eval-region)

(setq custom-file (concat user-emacs-directory "custom-set-variables.el"))
(load custom-file 'noerror)

(provide 'init.el)
;;; hic terminatur init.el
