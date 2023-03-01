;; -*- lexical-binding: t; -*-
;; SETQ AND DEFAULTS
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
      make-backup-files nil
      auto-save-default nil
      confirm-nonexistent-file-or-buffer nil
      show-paren-delay 0
      show-paren-style 'expression
      blink-matching-paren 'jump
      global-hl-line-sticky-flag t
      echo-keystrokes 0.01
      save-interprogram-paste-before-kill t
      confirm-kill-processes nil
      frame-title-format "%b - EMACS"
      eshell-destroy-buffer-when-process-dies t
      dired-listing-switches "-alv --group-directories-first"
      custom-file (concat user-emacs-directory "custom-set-variables.el")
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

(if (eq system-type 'windows-nt)
    (setq delete-by-moving-to-trash t
	  ediff-diff-program "c:/Program Files/Git/usr/bin/diff.exe"
	  ediff-diff3-program "c:/Program Files/Git/usr/bin/diff3.exe"
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
;; OPTIONS
(save-place-mode 1)
(fringe-mode 0)
(winner-mode 1)
(auto-fill-mode t)
(midnight-mode t)
(column-number-mode t)
(file-name-shadow-mode t)
(delete-selection-mode t)
(global-hl-line-mode t)
(global-prettify-symbols-mode t)
(global-display-line-numbers-mode t)

(let ((my-bg-col "#cae0a6"))
  (set-face-background 'show-paren-match nil)
  (set-face-underline 'show-paren-match t)
  (set-face-foreground 'mode-line "black")
  (set-face-foreground 'mode-line-inactive "darkgray")
  (set-face-background 'mode-line my-bg-col)
  (set-face-background 'mode-line-inactive my-bg-col)
  (add-to-list 'default-frame-alist '(background-color . my-bg-col)))

(unless (display-graphic-p)
  (xterm-mouse-mode 1))
