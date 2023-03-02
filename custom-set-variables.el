;; -*- lexical-binding: t; -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ledger-reports
   '(("assets" "%(binary) -f %(ledger-file) bal assets")
     ("income" "%(binary) -f %(ledger-file) bal income")
     ("expenses" "%(binary) -f %(ledger-file) bal expenses")
     ("liabilities" "%(binary) -f %(ledger-file) bal liabilities")
     ("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")))
 '(minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
 '(package-selected-packages
   '(magit browse-kill-ring use-package page-break-lines openwith gnuplot-mode expand-region company)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:foreground "black"))))
 '(mode-line-inactive ((t (:foreground "darkgray"))))
 '(show-paren-match ((t (:underline t)))))
