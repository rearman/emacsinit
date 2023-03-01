;; -*- lexical-binding: t; -*-
;; HOOKS
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'dired-mode-hook 'dired-hide-details-mode t)
(add-hook 'lisp-mode-hook (lambda () (setq tab-width 2)))
(add-hook 'emacs-lisp-mode-hook (lambda () (setq tab-width 2)))
(add-hook 'c-mode-common-hook (lambda ()
								(c-set-style "k&r")
								(setq tab-width 5
									  c-basic-offset 5
									  indent-tabs-mode t
									  comment-column 48)))


;; PUTS AND PUSHES
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
