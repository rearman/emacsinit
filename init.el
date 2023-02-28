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

;; SETUP FOR SHARED WINDOWS/WSL INIT
(if (eq system-type 'windows-nt)
    (progn (setq delete-by-moving-to-trash t
		 ediff-diff-program "c:/Program Files/Git/usr/bin/diff.exe"
		 ediff-diff3-program "c:/Program Files/Git/usr/bin/diff3.exe"
		 openwith-associations '(("\\.pdf\\'" "sumatrapdf" (file))
					 ("\\.xls\\'" "excel" (file))
					 ("\\.xlsx\\'" "excel" (file))
					 ("\\.doc\\'" "word" (file))
					 ("\\.docx\\'" "word" (file))
					 ("\\.adpro\\'" "PoductivitySuite" (file))))
	   (load (expand-file-name "win-defuns.el" user-emacs-directory)))
  (setq openwith-associations '(("\\.pdf\\'" "zathura" (file))
				("\\.xls\\'" "libreoffice" (file))
				("\\.xlsx\\'" "libreoffice" (file))
				("\\.doc\\'" "libreoffice" (file))
				("\\.docx\\'" "libreoffice" (file)))))

(load custom-file 'noerror)
