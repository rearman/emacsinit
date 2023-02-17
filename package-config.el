;; -*- lexical-binding: t; -*-

(require 'package)
(setq package-archives
      '(("elpa" . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")
	("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package company
  :ensure t
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
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package magit
  :ensure t
  :init
  (message "Loading Magit...")
  :config
  (message "Loaded Magit!")
  :bind (("C-x g" . magit-status)))

(use-package gnuplot-mode
  :ensure t)

(use-package openwith
  :ensure t
  :config
  (openwith-mode t))

(use-package s
  :ensure t)

(use-package dash
  :ensure t)

(use-package acme-theme
  :ensure t)
  ;;:config
  ;;(load-theme 'acme t))

(unless (eq system-type 'windows-nt)
  (use-package slime :ensure t)
  (use-package ledger-mode :ensure t))
