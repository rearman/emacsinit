;; -*- lexical-binding: t; -*-
;; ORG SETTINGS AND DEFUNS
(defun erfassen-zettel ()
  "Add a new zettel to the kasten.
Creates a new file <datestamp>-name.org in ~/org/kasten."
  (interactive)
  (let ((name (read-string "Zettel-Name: ")))
  (expand-file-name (format "%s-%s.org" (format-time-string "%Y-%m-%d-%H%M") name) "~/org/kasten/")))

(defun org-auto-archive ()
  "Automatically archive completed tasks in an org file.
Intended for use as an after-save-hook."
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "TODO=\"DONE\"|TODO=\"CANCELLED\""
   'file)
  (save-buffer))

(setq org-M-RET-may-split-line nil
      org-startup-folded t
      org-reverse-note-order t
      org-use-fast-todo-selection 'expert
      org-agenda-start-on-weekday nil
      org-catch-invisible-edits 'show
      org-directory "~/org"
      org-agenda-files '("~/org/"
			 "~/org/kasten/")
      org-refile-targets '((nil :maxlevel . 9)
			   (org-agenda-files :maxlevel . 9))
      org-default-notes-file (concat org-directory "/notes.org")
      safe-local-variable-values '((after-save-hook org-auto-archive))
      org-todo-keywords '((sequence "TODO(t@)"
				    "WAITING(w@)"
				    "IN-PROGRESS(i@)"
				    "DELEGATED(l@)"
				    "APPT(a@)" "|"
				    "DONE(d@)"
				    "CANCELLED(c@)"))
      org-capture-templates '(("n" "Note" entry (file+olp org-default-notes-file "Notes") "* %u %?")
			      ("t" "TODO" entry (file+olp org-default-notes-file "Tasks") "* TODO %? \n %u")
			      ("s" "Service" entry (file+olp org-default-notes-file "Service") "* TODO %? \n %u")
			      ("z" "Zettel" entry (file erfassen-zettel) "* %? ::"))
      org-agenda-custom-commands '(("n" "Agenda and all TODOs"
				    ((agenda "") (alltodo "")))
				   ("u" "Unscheduled TODOs"
				    alltodo "" ((org-agenda-skip-function
						 (lambda nil
						   (org-agenda-skip-entry-if 'scheduled
									     'deadline
									     'regexp "\n]+>")))
						(org-agenda-overriding-header "Unscheduled TODO entries: ")))))
