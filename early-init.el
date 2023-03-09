;;; early-init.el --- Emacs 27+ pre-initialization config -*- lexical-binding: t; -*-
;; 100MB garbage collection threshold during startup(default 800KB)
(setq gc-cons-threshold 100000000)
;; Drop to 50MB after init
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 50000000)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq default-directory "~/"
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-buffer-menu t
      initial-scratch-message nil
      server-client-instructions nil)

(add-to-list 'default-frame-alist '(background-color . "#cae0a6"))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(provide 'early-init)
;;; hic terminatur early-init.el
