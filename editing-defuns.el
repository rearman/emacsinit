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

(defun edit-init ()
  "Bring up init.el for editing."
  (interactive)
  (find-file shared-system-init))
