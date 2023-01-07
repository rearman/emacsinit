;; -*- lexical-binding: t; -*-

;; PACKAGES
;; NEW STRAIGHT.EL CONFIG
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package straight
  :custom (straight-use-package-by-default t))

(use-package company
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 3)
  (company-selection-wrap-around t)
  (company-dabbrev-other-buffers t)
  (company-dabbrev-code-other-buffers t)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case t)
  :config
  (global-company-mode t))

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package magit
  :init
  (message "Loading Magit...")
  :config
  (message "Loaded Magit!")
  :bind (("C-x g" . magit-status)))

(use-package gnuplot-mode)

(use-package openwith
  :config
  (openwith-mode t)
  (setq openwith-associations '(("\\.pdf\\'" "sumatrapdf" (file))
				("\\.xls\\'" "excel" (file))
				("\\.xlsx\\'" "excel" (file))
				("\\.doc\\'" "word" (file))
				("\\.docx\\'" "word" (file))
				("\\.adpro\\'" "PoductivitySuite" (file)))))
(use-package s)

(use-package dash)
