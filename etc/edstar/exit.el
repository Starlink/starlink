;;;###autoload
(defun exit ()
"
Purpose:
   Exit cleanly from emacs.

Description:
   The function saves all modified buffers which are visiting files and then
   kills the current emacs session.
"
   (interactive)

;;; Save all buffers and kill emacs.
   (save-buffers-kill-emacs t))

;;;###autoload
(defun quit ()
"
Purpose:
  Abort the current emacs session.

Description:
  The function aborts the current emacs session without saving the contents
  of any modified buffers. If modified buffers exist (and they are visiting
  files), then confirmation is requested.
"
  (interactive)

;;; Define local variables, setting l to the current list of buffers.
  (let (mod
	(l (buffer-list)))

;;; Loop to inspect each buffer.
    (while l

;;; Check if the buffer is visiting a file. If so, note if it is modified.
      (if (and (buffer-file-name (car l))
	       (buffer-modified-p (car l))) (setq mod t))

;;; Increment to inspect the next buffer.
      (setq l (cdr l)))

;;; Kill emacs if there were no modified buffers visiting files or if
;;; confirmation is given.
    (if (or (not mod)
	    (and mod
		 (yes-or-no-p "There are modified buffers. Quit anyway? ")))
	(kill-emacs t))))
