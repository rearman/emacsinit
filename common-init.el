;; -*- lexical-binding: t; -*-
;; PACKAGES
;; NEW STRAIGHT.EL CONFIG
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package company
  :straight t
  :custom
  (company-idle-delay 0.01)
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t)
  :config
  (global-company-mode t))

(use-package aggressive-indent
  :straight t
  :hook lisp-interaction-mode emacs-lisp-mode lisp-mode scheme-mode)

(use-package expand-region
  :straight t
  :bind (("C-=" . er/expand-region)))

(use-package magit
  :straight t
  :init
  (message "Loading Magit...")
  :config
  (message "Loaded Magit!")
  :bind (("C-x g". magit-status)))

;; OPTIONS
(ido-mode t)
(ido-everywhere t)
(recentf-mode t)
(menu-bar-mode -1)
(auto-fill-mode t)
(show-paren-mode t)
(delete-selection-mode t)
(global-hl-line-mode t)
(global-prettify-symbols-mode t)
(global-display-line-numbers-mode t)

;; HOOKS
(add-hook 'after-init-hook 'visual-line-mode)
(add-hook 'before-save-hook 'whitespace-cleanup)

;; SETQ AND DEFAULTS
(setq inhibit-startup-message t
      initial-scratch-message nil
      use-short-answers t
      make-backup-files nil
      auto-save-default nil
      show-paren-delay 0.01
      confirm-nonexistent-file-or-buffer nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      org-M-RET-may-split-line nil
      org-startup-folded t
      org-startup-indented t)

(setq-default indicate-empty-lines t
	      cursor-type 'bar
	      mode-line-format
	      (list
	       " " mode-line-modified
	       " %[" mode-line-buffer-identification "%] %l,%c %6 "
	       mode-line-misc-info
	       mode-line-end-spaces))

;; DEFUNS
(defun goto-line-with-feedback ()
  "Show line numbers, go to selected line, turn off line numbers.
If line numbers were already on, leave them on."
  (interactive)
  (unwind-protect
      (if (eq t display-line-numbers-mode)
	  (goto-line (read-number "Goto Line: "))
	(progn
	  (display-line-numbers-mode 1)
	  (goto-line (read-number "Goto Line: "))
	  (display-line-numbers-mode -1)))))

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
  "Create a new empty buffer."
  (interactive)
  (let ((newbuf (generate-new-buffer "untitled")))
    (switch-to-buffer newbuf)
    (setq buffer-offer-save t)
    newbuf))

(defun kill-current-buffer ()
  "Kill the current buffer instead of prompting."
  (interactive)
  (kill-buffer (current-buffer)))

(defun edit-init ()
  "Bring up init.el for editing."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun edit-common-init ()
  "Bring up the common windows/wsl init.
Assumes common-init is defined as a variable in the separate init."
  (interactive)
  (find-file common-init))

(load-file "./work-eqs.el")

;; PUTS AND PUSHES
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; BINDINGS
;; USER SPACE
(global-set-key (kbd "C-c ai") 'aggressive-indent-mode)
(global-set-key (kbd "C-c b") 'new-empty-buffer)
(global-set-key (kbd "C-c ei") 'edit-init)
(global-set-key (kbd "C-c ec") 'edit-common-init)
(global-set-key (kbd "C-c h") 'global-hl-line-mode)
(global-set-key (kbd "C-c n") 'display-line-numbers-mode)
(global-set-key (kbd "C-c r") 'recentf-open-files)
;; OVERRIDES
(global-set-key [remap goto-line] 'goto-line-with-feedback)
(global-set-key (kbd "M-j") 'backward-join-line)
(global-set-key (kbd "M-J") 'join-line)
(global-set-key (kbd "C-o") 'open-line-below)
(global-set-key (kbd "M-o") 'open-line-above)
(global-set-key (kbd "C-u") 'backward-kill-line)
(global-set-key (kbd "C-w") 'kill-bword-or-region)
(global-set-key (kbd "C-z") 'zap-up-to-char)
(global-set-key (kbd "C-M-z") 'eval-region)
(global-set-key (kbd "C-'") 'universal-argument)
(global-set-key (kbd "M-%") 'replace-regexp)
(global-set-key (kbd "C-M-%") 'query-replace-regexp)

(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))
