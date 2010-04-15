;;;###autoload
(defun edstar-save-buffers ()
"
Purpose:
   Save all modified buffers.

Description:
   The function writes the contents of all buffers which have been modified
   back to their associated file, if any.
"
  (interactive)

;;; Local variables.
  (let ((l (buffer-list)))

;;; Loop to inspect each buffer.
    (save-excursion
      (while l

;;; Check if the buffer is visiting a file. If so, save it.
	(if (and (buffer-file-name (car l))
		 (buffer-modified-p (car l)))
	    (progn
	      (set-buffer (car l))
	      (save-buffer)))

;;; Increment to inspect the next buffer.
	(setq l (cdr l))))))
