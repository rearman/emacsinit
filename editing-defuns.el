;; -*- lexical-binding: t; -*-
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

(defun new-empty-buffer ()
  "Create a new empty buffer.  Stolen from Xah."
  (interactive)
  (let ((newbuf (generate-new-buffer "untitled")))
    (switch-to-buffer newbuf)
    (setq buffer-offer-save t)
    newbuf))

(defun edit-defuns ()
  "Bring up editing-defuns.el for editing."
  (interactive)
  (find-file editing-defuns))

(defun edit-init ()
  "Bring up init.el for editing."
  (interactive)
  (find-file shared-system-init))

(defun edit-work-eqs ()
  "Bring up work-eqs.el for editing."
  (interactive)
  (find-file work-eqns))

(defun toggle-window-split ()
  "If two windows are open, toggle their split layout between vert. and horiz.
Stolen from http://whattheemacsd.com"
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(defun rotate-windows ()
  "Rotate your windows.
Stolen from http://whattheemacsd.com"
  (interactive)
  (cond ((not (> (count-windows)1))
	 (message "You can't rotate a single window!"))
	(t
	 (setq i 1)
	 (setq numWindows (count-windows))
	 (while  (< i numWindows)
	   (let* (
		  (w1 (elt (window-list) i))
		  (w2 (elt (window-list) (+ (% i numWindows) 1)))

		  (b1 (window-buffer w1))
		  (b2 (window-buffer w2))

		  (s1 (window-start w1))
		  (s2 (window-start w2))
		  )
	     (set-window-buffer w1  b2)
	     (set-window-buffer w2 b1)
	     (set-window-start w1 s2)
	     (set-window-start w2 s1)
	     (setq i (1+ i)))))))

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer.
Stolen from http://whattheemacsd.com"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))
