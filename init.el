;; -*- lexical-binding: t; -*-
;; SETUP FOR SHARED WINDOWS/WSL INIT
(if (eq system-type 'windows-nt)
    (progn (defvar work-eqns (expand-file-name "work-eqs.el" user-emacs-directory))
	   (add-to-list 'default-frame-alist '(background-color . "#cae0a6"))
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
  :bind (("C-x g" . magit-status)
	 :map magit-status-mode-map
	 ("q" . magit-quit-session)))

(use-package gnuplot-mode)

(use-package openwith
  :config
  (openwith-mode t)
  (setq openwith-associations '(("\\.pdf\\'" "sumatrapdf" (file))
				("\\.xls\\'" "excel" (file))
				("\\.xlsx\\'" "excel" (file))
				("\\.doc\\'" "word" (file))
				("\\.docx\\'" "word" (file)))))

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
      show-paren-delay 0
      show-paren-style 'expression
      blink-matching-paren 'jump
      global-hl-line-sticky-flag t
      confirm-nonexistent-file-or-buffer nil
      apropos-do-all t
      echo-keystrokes 0.01
      save-interprogram-paste-before-kill t)

;; ORG SETTINGS
(setq org-M-RET-may-split-line nil
      org-startup-folded t
      org-startup-indented t
      org-catch-invisible-edits 'show
      org-directory "~/org"
      org-default-notes-file (concat org-directory "/notes.org")
      org-service-notes-file (concat org-directory "/service.org")
      org-capture-templates '(("t" "Default TODO" entry (file org-default-notes-file)
			       "* TODO %?")
			      ("s" "Service Req." entry (file org-service-notes-file)
			       "* TODO %?")))

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
(column-number-mode t)
(global-hl-line-mode t)
(global-prettify-symbols-mode t)
(global-display-line-numbers-mode t)
(set-face-background 'show-paren-match nil)
(set-face-underline 'show-paren-match t)

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

(defun new-empty-buffer ()
  "Create a new empty buffer.  Stolen from Xah."
  (interactive)
  (let ((newbuf (generate-new-buffer "untitled")))
    (switch-to-buffer newbuf)
    (setq buffer-offer-save t)
    newbuf))

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

(defadvice magit-status (around magit-fullscreen activate)
  "Stolen from http://whattheemacsd.com"
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer.
Stolen from http://whattheemacsd.com"
  (interactive)
  (kill-matching-buffers "magit" nil t)
  (jump-to-register :magit-fullscreen))

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.
If point was already at that position, move point to beginning of line.
Stolen from BrettWitty's dotemacs github repo."
  (interactive "^")
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
	 (beginning-of-line))))

(defun set-cursor-by-mode ()
  "Change cursor type according to some minor modes.
bar normally, hbar in read-only, and box in overwrite.  Stolen from https://emacs-fu.blogspot.com."
  (cond
   (buffer-read-only
    (setq cursor-type 'hbar))
   (overwrite-mode
    (setq cursor-type 'box))
   (t
    (setq cursor-type 'bar))))

(defun go-into-hs-minor-mode ()
  "Sets bindings and options for hs-mode, then goes into it"
  (local-set-key (kbd "C-c <tab>") 'hs-toggle-hiding)
  (local-set-key (kbd "C-c H") 'hs-hide-all)
  (local-set-key (kbd "C-c S") 'hs-show-all)
  (hs-minor-mode t))

;; HOOKS
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'dired-mode-hook 'dired-hide-details-mode t)
(add-hook 'post-command-hook 'set-cursor-by-mode)
(add-hook 'c-mode-common-hook 'go-into-hs-minor-mode)
(add-hook 'lisp-mode-hook 'go-into-hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'go-into-hs-minor-mode)

;; PUTS AND PUSHES
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; BINDINGS
;; USER SPACE
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'buffer-menu-other-window)
(global-set-key (kbd "C-c i") 'edit-init)
(global-set-key (kbd "C-c n") 'new-empty-buffer)
(global-set-key (kbd "C-c r") 'rotate-windows)
(global-set-key (kbd "C-c s") 'eshell)
(global-set-key (kbd "C-c t") 'toggle-window-split)
(global-set-key (kbd "C-c w") 'edit-work-eqs)
(global-set-key (kbd "<f8>") 'recentf-open-files)
;; OVERRIDES
(global-set-key [remap move-beginning-of-line] 'smart-beginning-of-line)
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
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
