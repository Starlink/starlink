;;;###autoload
(defun res (&optional file)
"
Purpose:
   Reserve a file from the (sccs) repository.

Description:
   The function reserves a file from the (sccs) repository and reads it into
   a buffer for editing. The command \"sccs edit\" is used for reserving the
   file, unless an alternative command has been specified via tha EDSTAR_RES
   environment variable.

   If a buffer of the same name was previously in use it is over-written, but
   the editing position (point) is preserved.

Arguments:
   file (optional)
      A string giving the name of the file to be reserved (without any
      directory prefix). The file should exist in the SCCS repository and
      should not already be reserved. If this argument is not given, it is
      prompted for. The default is to use the name of the file being visited
      by the current buffer.
"
  (interactive)

;;; Define local variables.
  (let (found posn fullfile oldbuf res)

;;; Check if a file name argument was given. If not, then obtain the name of
;;; the file being visited by the current buffer (if any), removing any
;;; directory or version information.
    (if (or (not file) (string= file ""))
	(progn
	  (setq file (buffer-file-name))
	  (if file
	      (setq file (file-name-sans-versions
			  (file-name-nondirectory file))))

;;; Prompt for the name of the file to reserve, supplying the file name
;;; obtained above (if any) as the default.
	  (setq file (edstar-file-prompt "File to reserve: " file))

;;; If no file name was obtained, then report an error.
	  (if (string= file "")
	      (message "No file name given."))))

;;; If a file name has been obtained, we must now reserve it.
    (if (and file (not (string= file "")))
	(progn

;;; Save the current buffer and window display status.
	  (save-excursion
	    (save-window-excursion

;;; Obtain a buffer to display the results of reserving the file and make it
;;; the current buffer.
	      (set-buffer (setq buffer (get-buffer-create "*SDT commands*")))

;;; Erase any buffer contents and display it in a window.
	      (erase-buffer)
	      (display-buffer buffer)

;;; Construct the full name expected for the reserved file by prepending the
;;; default directory information.
	      (setq fullfile (expand-file-name file edstar-default-directory))

;;; See if a writable copy of the file already exists. If so, it may already
;;; be reserved (in any case, reservation will fail unless it is removed).
;;; Insert a message into the output buffer.
	      (if (and (file-exists-p fullfile) (file-writable-p fullfile))
		  (progn
		    (insert
		     (concat
		      "A writeable copy of \"" file "\" already exists in:\n"
		      "   " fullfile "\n\n"
		      "*** This file may already be reserved ***\n"))

;;; Prompt to see if this file should be edited. If so, then note that a
;;; "reserved" file has been found.
		    (if (y-or-n-p "Edit this file? ")
			(progn
			  (setq found t)

;;; See if a buffer already exists named after this file. If so, save the
;;; current buffer status and make this "old" buffer current.
			  (if (setq oldbuf (get-buffer file))
			      (save-excursion
				(set-buffer oldbuf)

;;; Remember the editing position in this buffer.
				(setq posn (point))

;;; If it is read-only, then kill it (otherwise it will simply be over-written
;;; when we visit the new file).
				(if buffer-read-only
				    (kill-buffer (current-buffer))))))))

;;; If a writable file does not already exist, then proceed with reserving it.
;;; Set the buffer's default directory to the global default (so that this will
;;; be used by the process reserving the file when it comes to create the
;;; reserved copy).
		(let ((default-directory edstar-default-directory))

;;; Obtain the reservation command, supplying a default if necessary.
		  (setq res (or (getenv "EDSTAR_RES") "sccs edit"))

;;; Enter the reservation command into the buffer and then create a shell
;;; process to execute this command. Output goes to the current buffer.
		  (insert (concat res " " file "\n"))
		  (call-process shell-file-name nil t t "-c"
				(concat res " " file))
		  (accept-process-output))

;;; Check that the reserved file exists and is modifiable. If so, note that
;;; reservation has succeeded.
		(if (and (file-exists-p fullfile) (file-writable-p fullfile))
		    (progn
		      (setq found t)

;;; See if a buffer already exists named after this file. If so, save the
;;; current buffer status and make this "old" buffer current.
		      (if (setq oldbuf (get-buffer file))
			  (save-excursion
			    (set-buffer oldbuf)

;;; Remember the editing position in this buffer.
			    (setq posn (point))

;;; If it is read-only, then kill it (otherwise it will simply be over-written
;;; when we visit the new file).
			    (if buffer-read-only
				(kill-buffer (current-buffer)))))

;;; Visit the new file. If a previous editing position was defined, then move
;;; to it.
		      (edstar-visit fullfile)
		      (if posn (goto-char posn))

;;; Give the user a chance to read the output from the reservation process
;;; before continuing.
		      (sit-for 3))

;;; If the reserved copy of the file could not be found, then something went
;;; wrong. Add an error message to the output buffer, and wait (indefinitely)
;;; while the user diagnoses the error.
		  (insert (concat "*** File \"" file
				  "\" could not be reserved ***\n"))
		  (sit-for 100000)))))

;;; At this point, all buffers and windows revert to their initial state, so
;;; if the reservation succeeded, re-visit the reserved file and select the
;;; required editing position (if defined).
	  (if found
	      (progn
		(edstar-visit fullfile)
		(if posn (goto-char posn))))))))

;;;###autoload
(defun repl (&optional file)
"
Purpose:
   Replace a file in the (sccs) repository.

Description:
   The function replaces a file in the (sccs) repository and kills any buffer
   which was previously visiting the file (the buffer is first saved if
   necessary). The command \"sccs delta\" is used for replacing the file
   unless an alternative has been supplied via the EDSTAR_REPL environment
   variable.

Arguments:
   file (optional)
      A string giving the name of the file to be replaced (without any
      directory prefix). The file should exist in the SCCS repository and
      should be reserved, with a writeable copy in the current default
      directory. If this argument is not given, it is prompted for. The
      default is to use the name of the file being visited by the current
      buffer.
"
  (interactive)

;;; Define local variables.
  (let ((fullfile)
        (ok nil)
        (oldbuf nil)
        (repl))

;;; Check if a file name argument was given. If not, then obtain the name of
;;; the file being visited by the current buffer (if any), removing any
;;; directory or version information.
    (if (or (not file) (string= file ""))
	(progn
	  (setq file (buffer-file-name))
	  (if file
	      (setq file (file-name-nondirectory
                          (file-name-sans-versions file))))

;;; Prompt for the name of the file to replace, supplying the file name
;;; obtained above (if any) as the default.
	  (setq file (edstar-file-prompt "File to replace: " file))

;;; If no file name was obtained, then report an error.
	  (if (string= file "")
	      (message "No file name given."))))

;;; If a file name has been obtained, we must now replace it.
    (if (and file (not (string= file "")))
	(progn

;;; Save the current buffer and window display status.
	  (save-excursion
	    (save-window-excursion

;;; Obtain a buffer to display the results of replacing the file and make it
;;; the current buffer.
	      (set-buffer (setq buffer (get-buffer-create "*SDT commands*")))

;;; Erase any buffer contents and display it in a window.
	      (erase-buffer)
	      (display-buffer buffer)

;;; Construct the full name expected for the reserved file by prepending the
;;; default directory information.
	      (setq fullfile (expand-file-name file edstar-default-directory))

;;; Check if any existing buffer is visiting this file. If so, save the
;;; current buffer status. Make the specified one current and save its
;;; contents.
	      (if (setq oldbuf (edstar-find-file-buffer fullfile))
		  (save-excursion
		    (set-buffer oldbuf)
		    (save-buffer)))

;;; Now check that the reserved file exists and is writeable. If not, then
;;; report an error.
	      (if (not (and (file-exists-p fullfile)
			    (file-writable-p fullfile)))
		  (message
		   (concat "File \"" file "\" does not exist or is not "
			   "writeable (not reserved)."))

;;; If the file appears to be reserved, then set the current buffer default
;;; directory appropriately (this will be used by the process running the
;;; repl command).
		(let ((default-directory edstar-default-directory))

;;; Obtain the replacement command, supplying a default if necessary.
		  (setq repl (or (getenv "EDSTAR_REPL") "sccs delta"))

;;; Enter the replacement command into the buffer and then create a shell
;;; process to execute this command. Output goes to the current buffer.
		  (insert (concat repl " " file "\n"))
		  (call-process shell-file-name nil t t "-c"
				(concat repl " " file))
		  (accept-process-output))

;;; When the process completes, check that the reserved file no longer exists.
;;; If it does, then add an error message to the output buffer.
		(if (file-exists-p fullfile)
		    (insert (concat "\n*** The file \"" file
				    "\" was not replaced successfully ***\n"))

;;; Note if successful and kill the buffer that was previously visiting the
;;; file.
		  (setq ok t)
		  (if oldbuf (kill-buffer oldbuf)))

;;; Ensure that the display is up to date and wait for the user to digest
;;; the information about the replacment operation. Wait indefinitely if
;;; there appears to have been an error.
		(if ok
                    (sit-for 3)
		  (sit-for 100000)))))))))
