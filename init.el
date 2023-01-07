;; -*- lexical-binding: t; -*-
;; SETUP FOR SHARED WINDOWS/WSL INIT
(if (eq system-type 'windows-nt)
    (progn (defvar work-eqns (expand-file-name "work-eqs.el" user-emacs-directory))
	   (setq delete-by-moving-to-trash t))
  nil)

(load work-eqns)

;; PACKAGES
;; NEW STRAIGHT.EL CONFIG
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package straight
  :custom (straight-use-package-by-default t))

(use-package company
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 3)
  (company-selection-wrap-around t)
  (company-dabbrev-other-buffers t)
  (company-dabbrev-code-other-buffers t)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case t)
  :config
  (global-company-mode t))

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package magit
  :init
  (message "Loading Magit...")
  :config
  (message "Loaded Magit!")
  :bind (("C-x g" . magit-status)))
	 ;; :map magit-status-mode-map
	 ;; ("q" . magit-quit-session)))

(use-package gnuplot-mode)

(use-package openwith
  :config
  (openwith-mode t)
  (setq openwith-associations '(("\\.pdf\\'" "sumatrapdf" (file))
				("\\.xls\\'" "excel" (file))
				("\\.xlsx\\'" "excel" (file))
				("\\.doc\\'" "word" (file))
				("\\.docx\\'" "word" (file))
				("\\.adpro\\'" "PoductivitySuite" (file)))))
(use-package s)

(use-package dash)

;; SETQ AND DEFAULTS
(setq-default indicate-empty-lines t
	      fill-column 80
	      read-file-name-completion-ignore-case t
	      read-buffer-completion-ignore-case t
	      cursor-type 'bar
	      cursor-in-non-selected-windows nil)

(setq default-directory "~/"
      inhibit-startup-message t
      initial-scratch-message nil
      server-client-instructions nil
      require-final-newline t
      use-short-answers t
      mode-line-compact t
      make-backup-files nil
      auto-save-default nil
      confirm-nonexistent-file-or-buffer nil
      show-paren-delay 0
      show-paren-style 'expression
      blink-matching-paren 'jump
      global-hl-line-sticky-flag t
      echo-keystrokes 0.01
      save-interprogram-paste-before-kill t)

;; OPTIONS
(recentf-mode t)
(fringe-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode t)
(auto-fill-mode t)
(midnight-mode t)
(mouse-avoidance-mode 'animate)
(pixel-scroll-mode t)
(column-number-mode t)
(global-hl-line-mode t)
(global-prettify-symbols-mode t)
(global-display-line-numbers-mode t)
(set-face-background 'show-paren-match nil)
(set-face-underline 'show-paren-match t)
(add-to-list 'default-frame-alist '(background-color . "#cae0a6"))

(unless (display-graphic-p)
  (xterm-mouse-mode 1))

;; DEFUNS
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

(defun edit-init ()
  "Bring up init.el for editing."
  (interactive)
  (find-file user-init-file))

(defun edit-work-eqs ()
  "Bring up work-eqs.el for editing."
  (interactive)
  (find-file work-eqns))

(defun toggle-window-split ()
  "If two windows are open, toggle their split layout between vert. and horiz.
Stolen from http://whattheemacsd.com"
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(defun rotate-windows ()
  "Rotate your windows.
Stolen from http://whattheemacsd.com"
  (interactive)
  (cond ((not (> (count-windows)1))
	 (message "You can't rotate a single window!"))
	(t
	 (setq i 1)
	 (setq numWindows (count-windows))
	 (while  (< i numWindows)
	   (let* (
		  (w1 (elt (window-list) i))
		  (w2 (elt (window-list) (+ (% i numWindows) 1)))

		  (b1 (window-buffer w1))
		  (b2 (window-buffer w2))

		  (s1 (window-start w1))
		  (s2 (window-start w2))
		  )
	     (set-window-buffer w1  b2)
	     (set-window-buffer w2 b1)
	     (set-window-start w1 s2)
	     (set-window-start w2 s1)
	     (setq i (1+ i)))))))

;; (defadvice magit-status (around magit-fullscreen activate)
;;   "Stolen from http://whattheemacsd.com"
;;   (window-configuration-to-register :magit-fullscreen)
;;   ad-do-it
;;   (delete-other-windows))
;;
;; (defun magit-quit-session ()
;;   "Restores the previous window configuration and kills the magit buffer.
;; Stolen from http://whattheemacsd.com"
;;   (interactive)
;;   (kill-matching-buffers "magit" nil t)
;;   (jump-to-register :magit-fullscreen))

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.
If point was already at that position, move point to beginning of line.
Stolen from BrettWitty's dotemacs github repo."
  (interactive "^")
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
	 (beginning-of-line))))

(defun unfuck-aveva-license ()
  "Remove the offending xml files when aveva can't find the license."
  (interactive)
  (let ((xml1 "c:/ProgramData/AVEVA/Licensing/License API2/Data/LocalAcquireInfo.xml")
	(xml2 "c:/ProgramData/AVEVA/Licensing/License API2/Data/LocalBackEndAcquireInfo.xml"))
    (when (file-exists-p xml1) (delete-file xml1))
    (when (file-exists-p xml2) (delete-file xml2))))

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

(defun slot/get-queries (&optional pairs)
  "Get multiple `query-replace' pairs from the user.
PAIRS is a list of replacement pairs of the form (FROM . TO)."
  (-let* (((from to delim arg)
	   (query-replace-read-args
	    (s-join " "
		    (-non-nil
		     (list "Query replace many"
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

(defun slot/query-replace-many
    (pairs &optional delimited start end backward region-noncontiguous-p)
  "Like `query-replace', but query for several replacements.
Query for replacement pairs until the users enters an empty
string (but see `slot/get-queries').

Refer to `query-replace' and `perform-replace' for what the other
arguments actually mean."
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
   :query :regexp
   delimited nil nil start end backward region-noncontiguous-p))

;; ORG SETTINGS AND DEFUNS
(defun erfassen-zettel ()
  "Add a new zettel to the kasten.
Creates a new file <datestamp>-name.org in ~/org/kasten."
  (interactive)
  (let ((name (read-string "Zettel-Name: ")))
  (expand-file-name (format "%s-%s.org" (format-time-string "%Y-%m-%d-%H%M") name) "~/org/kasten/")))

(defun org-auto-archive ()
  "Automatically archive completed tasks in an org file.
Intended for use as an after-save-hook."
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "TODO=\"DONE\"|TODO=\"CANCELLED\""
   'file)
  (save-buffer))

(setq org-M-RET-may-split-line nil
      org-startup-folded t
      org-reverse-note-order t
      org-use-fast-todo-selection 'expert
      org-agenda-start-on-weekday nil
      org-catch-invisible-edits 'show
      org-directory "~/org"
      org-agenda-files '("~/org/"
			 "~/org/kasten/")
      org-refile-targets '((nil :maxlevel . 9)
			   (org-agenda-files :maxlevel . 9))
      org-default-notes-file (concat org-directory "/notes.org")
      safe-local-variable-values '((after-save-hook org-auto-archive))
      org-todo-keywords '((sequence "TODO(t@)"
				    "WAITING(w@)"
				    "IN-PROGRESS(i@)"
				    "DELEGATED(l@)"
				    "APPT(a@)" "|"
				    "DONE(d@)"
				    "CANCELLED(c@)"))
      org-capture-templates '(("n" "Note" entry (file+olp org-default-notes-file "Notes") "* %u %?")
			      ("t" "TODO" entry (file+olp org-default-notes-file "Tasks") "* TODO %? \n %u")
			      ("s" "Service" entry (file+olp org-default-notes-file "Service") "* TODO %? \n %u")
			      ("z" "Zettel" entry (file erfassen-zettel) "* %? ::"))
      org-agenda-custom-commands '(("n" "Agenda and all TODOs"
				    ((agenda "") (alltodo "")))
				   ("u" "Unscheduled TODOs"
				    alltodo "" ((org-agenda-skip-function
						 (lambda nil
						   (org-agenda-skip-entry-if 'scheduled
									     'deadline
									     'regexp "\n]+>")))
						(org-agenda-overriding-header "Unscheduled TODO entries: ")))))

;; HOOKS
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'dired-mode-hook 'dired-hide-details-mode t)

;; PUTS AND PUSHES
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; BINDINGS
;; USER SPACE
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'buffer-menu-other-window)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c ei") 'edit-init)
(global-set-key (kbd "C-c r") 'rotate-windows)
(global-set-key (kbd "C-c s") 'eshell)
(global-set-key (kbd "C-c t") 'toggle-window-split)
(global-set-key (kbd "C-c w") 'edit-work-eqs)
(global-set-key (kbd "<f8>") 'recentf-open-files)
;; OVERRIDES
(global-set-key [remap move-beginning-of-line] 'smart-beginning-of-line)
(global-set-key (kbd "M-j") 'backward-join-line)
(global-set-key (kbd "M-S-j") 'join-line)
(global-set-key (kbd "C-S-k") 'kill-whole-line)
(global-set-key (kbd "C-o") 'open-line-below)
(global-set-key (kbd "M-o") 'open-line-above)
(global-set-key (kbd "M-s .") 'isearch-forward-thing-at-point)
(global-set-key (kbd "M-s M-.") 'isearch-forward-symbol-at-point)
(global-set-key (kbd "C-u") 'backward-kill-line)
(global-set-key (kbd "C-w") 'kill-bword-or-region)
(global-set-key (kbd "C-z") 'zap-up-to-char)
(global-set-key (kbd "C-M-z") 'eval-region)
(global-set-key (kbd "C-'") 'universal-argument)
(global-set-key (kbd "C-.") 'next-window-any-frame)
(global-set-key (kbd "C-,") 'previous-window-any-frame)
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-x .") 'next-buffer)
(global-set-key (kbd "C-x ,") 'previous-buffer)
(global-set-key (kbd "C-%") 'replace-regexp)
(global-set-key (kbd "M-%") 'replace-string)
(global-set-key (kbd "C-M-%") 'query-replace-regexp)
;; MODE-SPECIFIC
(add-hook 'eshell-mode-hook (lambda ()
			      (define-key eshell-mode-map (kbd ")") 'eshell-send-on-close-paren)))
(add-hook 'org-mode-hook (lambda ()
			   (define-key org-mode-map (kbd "C-'") nil)
			   (define-key org-mode-map (kbd "C-,") nil)))
(add-hook 'org-agenda-mode-hook (lambda ()
				  (define-key org-agenda-mode-map (kbd "C-'") nil)
				  (define-key org-agenda-mode-map (kbd "C-,") nil)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)) ;; Stolen from Xah
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:foreground "black" :background "gray"))))
 '(mode-line-inactive ((t (:foreground "black" :background "#cae0a6"))))
 )
