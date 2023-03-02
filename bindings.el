;; -*- lexical-binding: t; -*-
;; BINDINGS
;; USER SPACE - mode-specific-map C-c by default + fkeys
(define-key mode-specific-map "b" 'buffer-menu-other-window)
(define-key mode-specific-map "ee" (lambda () (interactive) (dired user-emacs-directory)))
(define-key mode-specific-map "ei" (lambda () (interactive) (find-file user-init-file)))
(define-key mode-specific-map "ep" (lambda () (interactive) (find-file user-packages)))
(define-key mode-specific-map "es" (lambda () (interactive) (find-file user-setqs)))
(define-key mode-specific-map "ew" (lambda () (interactive) (find-file user-work-eqs)))
(define-key mode-specific-map "ed" (lambda () (interactive) (find-file user-defuns)))
(define-key mode-specific-map "eh" (lambda () (interactive) (find-file user-hooks)))
(define-key mode-specific-map "eb" (lambda () (interactive) (find-file user-bindings)))
(define-key mode-specific-map "s" 'eshell)
(define-key mode-specific-map ";" 'comment-or-uncomment-region)
(define-key global-map [f5] 'scratch-only)
(define-key global-map [f8] 'recentf-open-files)
;; OVERRIDES - global-map
(global-set-key [remap move-beginning-of-line] 'smart-beginning-of-line)
(global-set-key "\C-o" 'open-line-below)
(global-set-key "\C-u" 'backward-kill-line)
(global-set-key "\C-w" 'kill-bword-or-region)
(global-set-key "\C-z" 'zap-up-to-char)
(global-set-key [?\C-'] 'universal-argument)
(global-set-key [?\C-%] 'replace-regexp)
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)
(global-set-key [C-left] 'previous-window-any-frame)
(global-set-key [C-right] 'next-window-any-frame)
(global-set-key [?\C-\S-K] 'kill-whole-line)
(global-set-key [?\C-\S-S] 'isearch-forward-regexp)
(global-set-key [?\C-\S-R] 'isearch-backward-regexp)
;; OVERRIDES - ctl-x-map
(define-key ctl-x-map "\C-b" 'buffer-menu)
(define-key ctl-x-map "r\S-B" 'bookmark-jump-other-window)
;; OVERRIDES - esc-map (M-*)
(define-key esc-map "j" 'backward-join-line)
(define-key esc-map "o" 'open-line-above)
(define-key esc-map "s." 'isearch-forward-thing-at-point)
(define-key esc-map "s\M-." 'isearch-forward-symbol-at-point)
(define-key esc-map "%" 'slot/replace-string-many)
(define-key esc-map "\C-z" 'eval-region)
(define-key esc-map [?\C-%] 'slot/query-replace-many)
(define-key esc-map [C-left] 'previous-buffer)
(define-key esc-map [C-right] 'next-buffer)
;; MODE-SPECIFIC
(add-hook 'eshell-mode-hook (lambda () (define-key eshell-mode-map ")" 'eshell-send-on-close-paren)))
