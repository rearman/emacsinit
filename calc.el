;;; Definition stored by Calc on Mon Jan  8 11:30:38 2024
(put 'calc-define 'calc-User-scale-analog '(progn
 (defun calc-User-scale-analog (arg) (interactive "P")
  (calc-execute-kbd-macro ["s s e m i n <return> - M-' 3 <tab> s s p m i
  n <return> - <tab> / <return> s r e m i n <return> s r p m i n
  <return> <escape> <tab> / -" nil] arg "zs"))
 (put 'calc-User-scale-analog 'calc-user-defn 't)
 (define-key calc-mode-map "zs" 'calc-User-scale-analog)
))
