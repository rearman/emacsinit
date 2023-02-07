;; -*- lexical-binding: t; -*-
;; DEFUNS
(defun open-line-below ()
  "Creates a new empty line below the current line."
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  "Creates a new empty line above the current line.
Can't go prev line first, edge case of beginning of buffer."
  (interactive)
  (beginning-of-line)
  (newline)
  (previous-line)
  (indent-for-tab-command))

(defun kill-bword-or-region ()
  "Kill region if active, otherwise kill back one word."
  (interactive)
  (if (region-active-p)
      (call-interactively 'kill-region)
    (call-interactively 'backward-kill-word)))

(defun backward-kill-line ()
  "Kill back to beginning of line from point."
  (interactive)
  (kill-line 0))

(defun backward-join-line ()
  "A wrapper for join-line to make it go in the right direction."
  (interactive)
  (join-line 0))

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.
If point was already at that position, move point to beginning of line.
Stolen from BrettWitty's dotemacs github repo."
  (interactive "^")
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
	 (beginning-of-line))))

(defun eshell-send-on-close-paren ()
  "Makes eshell act somewhat like genera.
Makes a closing paren execute the sexp.  Currently in test, look out for errors."
  (interactive)
  (insert-char ?\))
  (let ((p (point)))
    ;;(eshell-bol)
    ;;(syntax-ppss-flush-cache (point))
    ;;(goto-char p)
    (cond ((= 0 (car (syntax-ppss)))
	   (eshell-send-input))
	  ((< (car (syntax-ppss)) 0)
	   (message "Check flush-cache"))
	  ((t nil)))))

(defun slot/get-queries (&optional pairs)
  "Get multiple 'query-replace' pairs from the user.
PAIRS is a list of replacement pairs of the form (FROM . TO).
Stolen from https://tony-zorman.com/posts/query-replace/2022-08-06-query-replace-many.html"
  (-let* (((from to delim arg)
	   (query-replace-read-args
	    (s-join " "
		    (-non-nil
		     (list "Replacements"
			   (cond ((eq current-prefix-arg '-) "backward")
				 (current-prefix-arg         "word"))
			   (when (use-region-p) "in region"))))
	    nil))                       ; no regexp-flag
	  (from-to (cons (regexp-quote from)
			 (s-replace "\\" "\\\\" to))))
    ;; HACK: Since the default suggestion of replace.el will be
    ;; the last one we've entered, an empty string will give us
    ;; exactly that.  Instead of trying to fight against this,
    ;; use it in order to signal an exit.
    (if (-contains? pairs from-to)
	(list pairs delim arg)
      (slot/get-queries (push from-to pairs)))))

(defun slot/replace-string-many
    (pairs &optional delimited start end backward region-noncontiguous-p)
  "Like 'replace-string', but query for several replacements.
Query for replacement pairs until the users enters an empty
string (but see 'slot/get-queries').

Refer to 'query-replace' and 'perform-replace' for what the other
arguments actually mean.
Stolen from https://tony-zorman.com/posts/query-replace/2022-08-06-query-replace-many.html
Edited and renamed to remove the Query."
  (interactive
   (let ((common (slot/get-queries)))
     (list (nth 0 common) (nth 1 common)
	   (if (use-region-p) (region-beginning))
	   (if (use-region-p) (region-end))
	   (nth 2 common)
	   (if (use-region-p) (region-noncontiguous-p)))))
  (perform-replace
   (concat "\\(?:" (mapconcat #'car pairs "\\|") "\\)") ; build query
   (cons (lambda (pairs _count)
	   (cl-loop for (from . to) in pairs
		    when (string-match from (match-string 0))
		    return to))
	 pairs)
   nil :regexp
   delimited nil nil start end backward region-noncontiguous-p))

(defun slot/query-replace-many
    (pairs &optional delimited start end backward region-noncontiguous-p)
  "Like 'query-replace', but query for several replacements.
Query for replacement pairs until the users enters an empty
string (but see 'slot/get-queries').

Refer to 'query-replace' and 'perform-replace' for what the other
arguments actually mean.
Stolen from https://tony-zorman.com/posts/query-replace/2022-08-06-query-replace-many.html"
  (interactive
   (let ((common (slot/get-queries)))
     (list (nth 0 common) (nth 1 common)
	   (when (use-region-p) (region-beginning))
	   (when (use-region-p) (region-end))
	   (nth 2 common)
	   (when (use-region-p) (region-noncontiguous-p)))))
  (perform-replace
   (concat "\\(?:" (mapconcat #'car pairs "\\|") "\\)") ; build query
   (cons (lambda (pairs _count)
	   (cl-loop for (from . to) in pairs
		    when (string-match from (match-string 0))
		    return to))
	 pairs)
   :query :regexp
   delimited nil nil start end backward region-noncontiguous-p))

(defun edit-keymap-planck ()
  "Bring up planck keymap for editing."
  (interactive)
  (find-file "~/qmk_firmware/keyboards/planck/keymaps/ehrman/keymap.c"))

(defun edit-keymap-tokyo60 ()
  "Bring up tokyo60 keymap for editing."
  (interactive)
  (find-file "~/qmk_firmware/keyboards/tokyokeyboard/tokyo60/keymaps/ehrman/keymap.c"))

(defun scratch-only ()
  "Bring up the scratch buffer as the only visible buffer."
  (interactive)
  (switch-to-buffer "*scratch*")
  (delete-other-windows))
