;; -*- lexical-binding: t; -*-
;; SETUP FOR SHARED WINDOWS/WSL INIT
(if (eq system-type 'windows-nt)
    (progn (setq delete-by-moving-to-trash t))
  nil)

(load (expand-file-name "config/work-eqs.el" user-emacs-directory))
(load (expand-file-name "config/package-config.el" user-emacs-directory))
(load (expand-file-name "config/setq-defaults.el" user-emacs-directory))
(load (expand-file-name "config/defuns.el" user-emacs-directory))
(load (expand-file-name "config/org-setup.el" user-emacs-directory))
(load (expand-file-name "config/hooks-puts.el" user-emacs-directory))
(load (expand-file-name "config/bindings.el" user-emacs-directory))

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
