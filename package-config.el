;; -*- lexical-binding: t; -*-

(require 'package)
(setq package-archives
      '(("elpa" . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")
	("melpa" . "https://melpa.org/packages/")))

(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)

(unless (eq system-type 'windows-nt)
  (use-package slime)
  (use-package ledger-mode))

(use-package s)
(use-package dash)
(use-package gnuplot-mode)

(use-package expand-region
  :bind
  (("C-=" . er/expand-region)))

(use-package popup-kill-ring
  :bind
  ("M-y" . popup-kill-ring))

(use-package openwith
  :config
  (openwith-mode t))

(use-package page-break-lines
  :init
  (global-page-break-lines-mode))

(use-package magit
  :init
  (message "Loading Magit...")
  :config
  (message "Loaded Magit!")
  :bind
  (("C-x g" . magit-status)))

(use-package auto-package-update
  :if (not (daemonp))
  :custom
  (auto-package-update-interval 7) ;; in days
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  :config
  (auto-package-update-maybe))

(use-package recentf
  :ensure nil
  :hook
  (after-init . recentf-mode)
  :custom
  (recentf-auto-cleanup "05:00am")
  (recentf-max-saved-items 200)
  (recentf-exclude '((expand-file-name package-user-dir)
		     ".cache"
		     ".cask"
		     ".elfeed"
		     "bookmarks"
		     "cache"
		     "ido.*"
		     "recentf")))

(use-package company
  :custom
  (company-idle-delay 0)
  (company-tooltip-idle-delay 10)
  (company-minimum-prefix-length 1)
  (company-require-match nil)
  (company-show-quick-access t)
  (company-format-margin-function 'company-text-icons-margin)
  (company-text-face-extra-attributes '(:slant italic))
  (company-selection-wrap-around t)
  (company-tooltip-flip-when-above t)
  (company-insertion-on-trigger nil)
  (company-dabbrev-other-buffers t)
  (company-dabbrev-code-other-buffers t)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case 'keep-prefix)
  (company-frontends '(company-pseudo-tooltip-unless-just-one-frontend-with-delay
		       company-preview-frontend
		       company-echo-metadata-frontend))
  :config
  (global-company-mode t)
  :bind
  (:map company-active-map
	("<tab>" . (lambda ()
		     (interactive)
		     (let ((company-tooltip-idle-delay 0.0))
		       (company-complete)
		       (and company-candidates
			    (company-call-frontends 'post-command)))))))

(use-package org
  :ensure nil
  :custom
  (org-M-RET-may-split-line nil)
  (org-reverse-note-order t)
  (org-use-fast-todo-selection 'expert)
  (org-agenda-start-on-weekday nil)
  (org-catch-invisible-edits 'show)
  (org-directory "~/org")
  (org-agenda-files '("~/org/"
		      "~/org/kasten/"))
  (org-refile-targets '((nil :maxlevel . 9)
			(org-agenda-files :maxlevel . 9)))
  (org-default-notes-file (concat org-directory "/notes.org"))
  (safe-local-variable-values '((after-save-hook org-auto-archive)))
  (org-todo-keywords '((sequence "TODO(t@)"
				 "WAITING(w@)"
				 "IN-PROGRESS(i@)"
				 "APPT(a@)" "|"
				 "DELEGATED(l@)"
				 "DONE(d@)"
				 "CANCELLED(c@)")))
  (org-capture-templates '(("n" "Note" entry (file+olp org-default-notes-file "Notes") "* %u %?")
			   ("t" "TODO" entry (file+olp org-default-notes-file "Tasks") "* TODO %? \n %u")
			   ("s" "Service" entry (file+olp org-default-notes-file "Service") "* TODO %? \n %u")
			   ("z" "Zettel" entry (file erfassen-zettel) "* %? ::")))
  (org-agenda-custom-commands '(("n" "Agenda and all TODOs"
				 ((agenda "") (alltodo "")))
				("u" "Unscheduled TODOs"
				 alltodo "" ((org-agenda-skip-function
					      (lambda nil
						(org-agenda-skip-entry-if 'scheduled
									  'deadline
									  'regexp "\n]+>")))
					     (org-agenda-overriding-header "Unscheduled TODO entries: ")))))
  :config
  (defun erfassen-zettel ()
    "Add a new zettel to the kasten.
Creates a new file <datestamp>.org in ~/org/kasten."
    (interactive)
    (expand-file-name (format "%s.org" (format-time-string "%Y-%m-%d-%H%M")) "~/org/kasten/"))

  (defun org-auto-archive ()
    "Automatically archive completed tasks in an org file.
Intended for use as an after-save-hook."
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
     "TODO=\"DONE\"|TODO=\"CANCELLED\"|TODO=\"DELEGATED\""
     'file)
    (save-buffer)))
