;; -*- lexical-binding: t; -*-
(defvar common-init "~/.emacs.d/common-init.el")
(load-file common-init)
(load-file "~/.emacs.d/work-eqs.el")

;; WIN GUI SPECIFIC OPTIONS
(tool-bar-mode -1)
(set-scroll-bar-mode 'left)
(add-to-list 'default-frame-alist '(background-color . "#cae0a6"))

;; HOOKS

;; SETQ AND DEFAULTS
(setq delete-by-moving-to-trash t)

;; DEFUNS

;; PUTS AND PUSHES

;; BINDINGS

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:foreground "black" :background "gray"))))
 '(mode-line-inactive ((t (:foreground "black" :background "#cae0a6"))))
 )
