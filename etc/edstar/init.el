(defvar edstar-default-directory)
;;;+
;;; Name:
;;;    edstar-init

;;; Type of Module:
;;;    Emacs lisp package

;;; Purpose:
;;;    Establish an initialisation function for EDSTAR.

;;; Invocation:
;;;    (load "edstar-init")

;;; Description:
;;;    This package defines initialisation functions which should be
;;;    executed whenever an EDSTAR editing session is started or resumed (they
;;;    tidy up after suspension of EDSTAR and set the default directory to
;;;    match that of the parent process, in case this has changed). Loading
;;;    this package establishes the initialisation function on the
;;;    suspend-resume-hook list, so that it will be executed when necessary.
;;;    It also executes it for the first time (if it has not already been
;;;    executed).

;;; History:
;;;    12-JUL-1994 (RFWS):
;;;       Original version.
;;;    14-SEP-1995 (RFWS):
;;;       Added code to handle the sentinel process.
;;;    {enter_further_changes_here}

;;;-

;;; Private Variables:
(defvar edstar-initialised nil
  "Records whether the edstar-init initialisation function has been run.")

;;;.

;;; Define initialisation function.
;;; ==============================
(defun edstar-init ()
"
Purpose:
   Perform global initialisation for EDSTAR.

Description:
   This function should be invoked when EDSTAR is first started and again whenever
   editing is resumed following suspension (using the \"edstar-detach\" command).
   It initialises the \"default directory\" used to search for new files to be the
   same as that of the parent process.
"

;;; Check if an initialisation file exists. If so, we will read the default
;;; directory name from it.
   (save-excursion
     (let (initfile buf)
       (setq initfile (concat "/tmp/edstar_" (getenv "EDSTAR_JOB") "_init"))
       (if (file-exists-p initfile)
	   (progn

;;; Create a temporary buffer.
	     (setq buf (get-buffer-create " edstar-init"))

;;; Make the buffer current.
	     (set-buffer buf)

;;; Read in the contents of the initialisation file.
	     (insert-file-contents initfile)

;;; Set the edstar-default-directory global variable to the value given in this
;;; file (as written by the parent process prior to resuming editing). Omit
;;; the final newline.
	     (setq edstar-default-directory
		   (file-name-as-directory
		    (buffer-substring 1 (- (point-max) 1))))

;;; Kill the temporary buffer and delete the initialisation file.
	     (kill-buffer buf)
	     (delete-file initfile))))))

;;; Establish the hook.
;;; ==================
;;; Make the above function execute whenever emacs is resumed after suspension.
(add-hook 'suspend-resume-hook 'edstar-init)

;;; Variable to store the sentinel process.
(defvar edstar-sentinel-process nil)

;;; Function to start the sentinel process.
(defun edstar-start-sentinel ()

;;; Start a sentinel process that just sleeps (other processes can then look
;;; at it to see if this edstar job is still running).
  (setq edstar-sentinel-process
	(start-process-shell-command "sentinel" "junk"
                                     (concat (getenv "EDSTAR_DIR")
                                             "/sentinel.sh")))

;;; Ensure the process dies quietly when edstar terminates.
  (process-kill-without-query edstar-sentinel-process)

;;; If running on a windows system, set a process sentinel function to
;;; repond to the sentinel process's changes of state. This will consist of
;;; noticing when it gets killed by the parent to request that edstar
;;; re-initialise itself.
  (if window-system
      (set-process-sentinel edstar-sentinel-process 'edstar-resume-windows))

;;; Create a flag file in /tmp named after this edstar job and write the
;;; sentinel process ID into it. This lets other processes know that this
;;; edstar job exists.
  (write-region (concat (int-to-string
			 (process-id edstar-sentinel-process)) "\n") 0
			 (concat "/tmp/edstar_" (getenv "EDSTAR_JOB")
				 "_pid")
			 nil 'nomsg))

;;; Define process sentinel function. This gets called on windows systems if
;;; the parent process kills the sentinel process associated with this edstar
;;; job.
(defun edstar-resume-windows (process event)

;;; Re-initialise edstar, de-iconify the frame and re-start the sentinel
;;; process.
  (edstar-init)
  (raise-frame (selected-frame))
  (edstar-start-sentinel))

;;; Perform initial startup.
;;; =======================
;;; Perform EDSTAR initialisation and start the sentinel process.
(edstar-init)
(edstar-start-sentinel)
