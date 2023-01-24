;; -*- lexical-binding: t; -*-

(load (expand-file-name "package-config.el" user-emacs-directory))
(load (expand-file-name "setq-defaults.el" user-emacs-directory))
(load (expand-file-name "work-eqs.el" user-emacs-directory))
(load (expand-file-name "defuns.el" user-emacs-directory))
(load (expand-file-name "org-setup.el" user-emacs-directory))
(load (expand-file-name "hooks-puts.el" user-emacs-directory))
(load (expand-file-name "bindings.el" user-emacs-directory))

;; SETUP FOR SHARED WINDOWS/WSL INIT
(if (eq system-type 'windows-nt)
    (progn (setq delete-by-moving-to-trash t
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ledger-reports
   '(("assets" "%(binary) -f %(ledger-file) bal assets")
     ("income" "%(binary) -f %(ledger-file) bal income")
     ("expenses" "%(binary) -f %(ledger-file) bal expenses")
     ("liabilities" "%(binary) -f %(ledger-file) bal liabilities")
     ("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")))
 '(minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
 '(package-selected-packages
   '(ledger-mode s openwith gnuplot-mode magit expand-region company use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:foreground "black" :background "gray"))))
 '(mode-line-inactive ((t (:foreground "black" :background "#cae0a6")))))
