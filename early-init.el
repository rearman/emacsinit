;;; early-init.el --- Emacs 27+ pre-initialization config -*- lexical-binding: t; -*-
;; GC threshold to max during startup(default 800KB)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
;; Drop to 25MiB after init
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold (* 1024 1024 25)
					 gc-cons-percentage 0.1)))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

(setq default-directory "~/"
      inhibit-startup-screen t
      inhibit-startup-buffer-menu t
      initial-scratch-message (message ";;; Emacs loaded in %s.\n\n" (emacs-init-time))
      server-client-instructions nil
      frame-resize-pixelwise t)

(setq default-frame-alist '((background-color . "#cae0a6")
			    (fullscreen . maximized)
			    (vertical-scroll-bars . nil)))

;;; hic terminatur early-init.el
