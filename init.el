;;; This file replaces itself with the real config at first run
;; We Can't tangle without org!
(require 'org)
;; Open the config...
(find-file (concat user-emacs-directory "init.org"))
;; Tangle it...
(org-babel-tangle)
;; Load it.
(load-file (concat user-emacs-directory "early-init.el"))
(load-file (concat user-emacs-directory "init.el"))
