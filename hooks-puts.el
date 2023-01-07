;; -*- lexical-binding: t; -*-
;; HOOKS
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'dired-mode-hook 'dired-hide-details-mode t)

;; PUTS AND PUSHES
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
