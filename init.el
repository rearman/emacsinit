;; -*- lexical-binding: t; -*-
;; SETUP FOR SHARED WINDOWS/WSL INIT
(if (eq system-type 'windows-nt)
    (progn (defvar shared-system-init (expand-file-name "init.el" user-emacs-directory))
	   (defvar work-eqns (expand-file-name "work-eqs.el" user-emacs-directory))
	   (defvar editing-defuns (expand-file-name "editing-defuns.el" user-emacs-directory))
	   (add-to-list 'default-frame-alist '(background-color . "#cae0a6"))
	   (setq delete-by-moving-to-trash t))
  nil)

(load-file work-eqns)
(load-file editing-defuns)

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

(use-package gnuplot-mode)

;; OPTIONS
(recentf-mode t)
(fringe-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(auto-fill-mode t)
(show-paren-mode t)
(column-number-mode t)
(set-scroll-bar-mode 'left)
(global-hl-line-mode t)
(global-prettify-symbols-mode t)
(global-display-line-numbers-mode t)

(unless (display-graphic-p)
  (xterm-mouse-mode 1))

;; HOOKS
(add-hook 'before-save-hook 'whitespace-cleanup)

;; SETQ AND DEFAULTS
(setq default-directory "~/"
      inhibit-startup-message t
      initial-scratch-message nil
      require-final-newline t
      use-short-answers t
      mode-line-compact t
      make-backup-files nil
      auto-save-default nil
      show-paren-delay 0
      show-paren-style 'mixed
      confirm-nonexistent-file-or-buffer nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      org-M-RET-may-split-line nil
      org-startup-folded t
      org-startup-indented t
      mouse-autoselect-window t)

(setq-default indicate-empty-lines t
	      show-trailing-whitespace t
	      read-file-name-completion-ignore-case t
	      read-buffer-completion-ignore-case t
	      cursor-type 'bar
	      cursor-in-non-selected-windows nil)

;; PUTS AND PUSHES
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; BINDINGS
;; USER SPACE
(global-set-key (kbd "C-c b") 'buffer-menu-other-window)
(global-set-key (kbd "C-c ed") 'edit-defuns)
(global-set-key (kbd "C-c ei") 'edit-init)
(global-set-key (kbd "C-c ew") 'edit-work-eqs)
(global-set-key (kbd "C-c r") 'recentf-open-files)
;; OVERRIDES
(global-set-key (kbd "M-j") 'backward-join-line)
(global-set-key (kbd "M-J") 'join-line)
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
(global-set-key (kbd "M-%") 'replace-regexp)
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
