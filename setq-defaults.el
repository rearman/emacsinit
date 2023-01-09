;; -*- lexical-binding: t; -*-
;; SETQ AND DEFAULTS
(setq-default indicate-empty-lines t
	      fill-column 80
	      read-file-name-completion-ignore-case t
	      read-buffer-completion-ignore-case t
	      cursor-type 'bar
	      cursor-in-non-selected-windows nil)

(setq require-final-newline t
      ring-bell-function 'ignore
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
(show-paren-mode t)
(auto-fill-mode t)
(midnight-mode t)
(mouse-avoidance-mode 'animate)
(column-number-mode t)
(delete-selection-mode t)
(global-hl-line-mode t)
(global-prettify-symbols-mode t)
(global-display-line-numbers-mode t)
(set-face-background 'show-paren-match nil)
(set-face-underline 'show-paren-match t)
(add-to-list 'default-frame-alist '(background-color . "#cae0a6"))

(unless (display-graphic-p)
  (xterm-mouse-mode 1))
