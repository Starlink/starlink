;;;###autoload
(defun edstar-check-readonly-dir ()
"
Purpose:
   Make buffers visiting readonly directories readonly.

Description:
   The function tests whether the current buffer is visiting a file which
   resides in one of the directories appearing in the list given by the
   translation of the environment variable EDSTAR_READONLY (as stored in the
   edstar-readonly list). If it is, the buffer is made readonly.
"

;;; Local variables.
  (let (file rdlist done dir thisdir)

;;; Get the name of the file being visited. If no file is being visited, there
;;; is nothing to do.
    (setq file (buffer-file-name))
    (if file
	(progn

;;; Get the expanded name of the directory containing the visited file,
;;; following any soft links.
	  (setq thisdir (file-name-directory
                         (file-truename
                          (expand-file-name file edstar-default-directory))))

;;; Get a list of the directories specified by the EDSTAR_READONLY variable
;;; and loop to test each directory.
	  (setq rdlist edstar-readonly)
	  (while (and rdlist (not done))

;;; Expand each directory name to get it in standard form, following any
;;; soft links.
	    (setq dir (file-name-as-directory
                       (file-truename
                        (expand-file-name (car rdlist)
                                          edstar-default-directory))))

;;; If the file's directory appears on the readonly list, make the buffer
;;; readonly and note we have finished.
	    (if (string= dir thisdir)
		(progn
		  (setq buffer-read-only t)
		  (setq done t)))

;;; Move on to test the next directory.
	    (setq rdlist (cdr rdlist)))))))

;;; Add edstar-check-readonly-dir to the hooks run when a file is found.
(add-hook 'find-file-hooks 'edstar-check-readonly-dir)
