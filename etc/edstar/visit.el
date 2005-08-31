;;;###autoload (defvar edstar-initial-string nil)

;;;###autoload
(defun goto-file (&optional file)
"
Purpose:
   Visit a file in a buffer, using EDSTAR_PATH to search for it.

Description:
   The function opens a file and reads it into a buffer, making that buffer
   current. In searching for the file, it uses the directory search path
   specified by the EDSTAR_PATH environment variable. If the file is not
   found, a new file is created.

Arguments:
   file
      The name of the file to be visited, as string. If nil is given the file
      will be prompted for.
"
   (interactive)

;;; Visit the file.
   (edstar-visit file))

;;;###autoload
(defun edstar-visit (&optional file)
"
Purpose:
   Visit a file in a buffer, using EDSTAR_PATH to search for it.

Description:
   The function opens a file and reads it into a buffer, making that buffer
   current. In searching for the file, it uses the directory search path
   specified by the EDSTAR_PATH environment variable. If the file is not
   found, a new file is created.

Arguments:
   file
      The name of the file to be visited, as a string. If nil is given the
      file will be prompted for.

Notes:
   This function is not interactively callable; goto-file provides the user
   callable interface.
"

;;; If no file name was given, then prompt for it.
;   (if (not file)
;       (setq file (edstar-file-prompt "File to edit: " edstar-file-prefix)))
   (if (not file)
       (setq file (edstar-file-prompt "File to edit: ")))

;;; Check if the file name is blank. If so, then don't do anything.
   (if (= 0 (length file)) (message "No file name given.")

;;; Define local variables.
     (let (fullfile)

;;; Search for the file, obtaining is full name if found.
       (if (setq fullfile (edstar-find file))

;;; If the file was found, then visit it.
	   (switch-to-buffer (find-file-noselect fullfile))

;;; If the file doesn't exist, then visit it as new file after first
;;; performing environment variable substitution and expanding to an
;;; absolute file name using edstar-default-directory.
	 (switch-to-buffer (find-file-noselect (expand-file-name
						(substitute-in-file-name file)
						edstar-default-directory)))

;;; If the new buffer is not readonly, is empty, and is unmodified, and if
;;; an initial buffer string is available, then insert it into the new buffer
;;; and move to the first placeholder (the initial string should contain a
;;; placeholder)..
	 (if (and (not buffer-read-only)
		  (= (point-min) (point-max))
		  (not (buffer-modified-p))
		  edstar-initial-string)
	     (progn
	       (save-excursion
		 (insert edstar-initial-string))
	       (edstar-best-place t)

;;; Ignore this modification, to prevent the buffer being written until it
;;; has actually been edited.
	       (set-buffer-modified-p nil)))))))
