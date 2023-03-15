;; We Can't tangle without org!
(require 'org)
;; Create the real config...
(org-babel-tangle (concat user-emacs-directory "init.org"))
;; Load it...
(load-file (concat user-emacs-directory "init.el"))
;; And compile it
(byte-compile-file (concat user-emacs-directory "init.el"))
