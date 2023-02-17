;; -*- lexical-binding: t; -*-
;; BINDINGS
;; USER SPACE
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'buffer-menu-other-window)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c ei") (lambda () (interactive) (find-file user-init-file)))
(global-set-key (kbd "C-c ep") (lambda () (interactive) (find-file (expand-file-name "package-config.el" user-emacs-directory))))
(global-set-key (kbd "C-c es") (lambda () (interactive) (find-file (expand-file-name "setq-defaults.el" user-emacs-directory))))
(global-set-key (kbd "C-c ew") (lambda () (interactive) (find-file (expand-file-name "work-eqs.el" user-emacs-directory))))
(global-set-key (kbd "C-c ed") (lambda () (interactive) (find-file (expand-file-name "defuns.el" user-emacs-directory))))
(global-set-key (kbd "C-c eo") (lambda () (interactive) (find-file (expand-file-name "org-setup.el" user-emacs-directory))))
(global-set-key (kbd "C-c eh") (lambda () (interactive) (find-file (expand-file-name "hooks-puts.el" user-emacs-directory))))
(global-set-key (kbd "C-c eb") (lambda () (interactive) (find-file (expand-file-name "bindings.el" user-emacs-directory))))
(global-set-key (kbd "C-c s") 'eshell)
(global-set-key (kbd "<f5>") 'scratch-only)
(global-set-key (kbd "<f8>") 'recentf-open-files)
;; OVERRIDES
(global-set-key [remap move-beginning-of-line] 'smart-beginning-of-line)
(global-set-key (kbd "M-j") 'backward-join-line)
(global-set-key (kbd "C-S-k") 'kill-whole-line)
(global-set-key (kbd "C-o") 'open-line-below)
(global-set-key (kbd "M-o") 'open-line-above)
(global-set-key (kbd "M-s .") 'isearch-forward-thing-at-point)
(global-set-key (kbd "M-s M-.") 'isearch-forward-symbol-at-point)
(global-set-key (kbd "C-u") 'backward-kill-line)
(global-set-key (kbd "C-w") 'kill-bword-or-region)
(global-set-key (kbd "C-z") 'zap-up-to-char)
(global-set-key (kbd "C-M-z") 'eval-region)
(global-set-key (kbd "C-'") 'universal-argument)
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-%") 'replace-regexp)
(global-set-key (kbd "M-%") 'slot/replace-string-many)
(global-set-key (kbd "C-M-%") 'slot/query-replace-many)
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)
(global-set-key (kbd "C-<left>") 'previous-window-any-frame)
(global-set-key (kbd "C-<right>") 'next-window-any-frame)
(global-set-key (kbd "C-M-<left>") 'previous-buffer)
(global-set-key (kbd "C-M-<right>") 'next-buffer)
;; MODE-SPECIFIC
(add-hook 'eshell-mode-hook (lambda ()
			      (define-key eshell-mode-map (kbd ")") 'eshell-send-on-close-paren)))
(add-hook 'org-mode-hook (lambda ()
			   (define-key org-mode-map (kbd "C-'") nil)
			   (define-key org-mode-map (kbd "C-,") nil)))
(add-hook 'org-agenda-mode-hook (lambda ()
				  (define-key org-agenda-mode-map (kbd "C-'") nil)
				  (define-key org-agenda-mode-map (kbd "C-,") nil)))
