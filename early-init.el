;;; early-init.el --- Emacs 27+ pre-initialization config

;;; Code:

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
