;; -*- lexical-binding: t; -*-

(defvar user-packages (expand-file-name "package-config.el" user-emacs-directory))
(defvar user-setqs (expand-file-name "setq-defaults.el" user-emacs-directory))
(defvar user-work-eqs (expand-file-name "work-eqs.el" user-emacs-directory))
(defvar user-defuns (expand-file-name "defuns.el" user-emacs-directory))
(defvar user-hooks (expand-file-name "hooks-puts.el" user-emacs-directory))
(defvar user-bindings (expand-file-name "bindings.el" user-emacs-directory))

(load user-packages)
(load user-setqs)
(load user-work-eqs)
(load user-defuns)
(load user-hooks)
(load user-bindings)

(when (eq system-type 'windows-nt)
  (load (expand-file-name "win-defuns.el" user-emacs-directory)))

(load custom-file 'noerror)
