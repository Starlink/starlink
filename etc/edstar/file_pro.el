;;; Initialise the global file prefix string (updated by edstar-file-prompt).
(defvar edstar-file-prefix "")

;;;###autoload
(defun edstar-file-prompt (prompt &optional prefix)
"
Purpose:
   Prompt for the name of a file to edit.

Description:
   The function prompts for the name of a file to edit, using the minibuffer,
   and returns the name supplied as a string. The file need not necessarily
   exist, but file name completion is provided, based on the set of files
   accessible in directories on the EDSTAR_PATH search path. Any files
   considered unsuitable for editing \(e.g. with file types specified in the
   EDSTAR_IGNORE_FILES environment variable\) are excluded when completing
   file names.

Arguments:
   prompt
      The string to be used to prompt the user.
   prefix (optional)
      An initial string to be entered into the minimuffer as a prefix for
      the file name.

Notes:
   A file name prefix, derived from the file name obtained, will be entered
   into the global variable edstar-file-prefix by this routine, for possible
   use as the \"prefix\" argument in subsequent invocations.
"

;;; Define local variables. Note that filelist and laststr are used by the
;;; edstar-complete-file function and must be initialised to nil here.
  (let (result filelist laststr)

;;; Clear the global file name prefix string.
    (setq edstar-file-prefix "")

;;; Perform a completing read on the minibuffer, supplying a function to
;;; perform completion and an initial prefix string.
    (setq result
	  (completing-read prompt 'edstar-file-complete nil nil prefix))

;;; Try to extract a prefix (ending in "_" or "-") from the resulting string.
;;; If successful, save this to use as a prefix for future use (e.g. in the
;;; next prompt).
    (if (string-match "\\([^-_ \t]*\\(_\\|-\\)\\).*" result)
	(setq edstar-file-prefix
	      (substring result (match-beginning 1) (match-end 1))))

;;; Return the result.
    result))

;;;###autoload
(defun edstar-file-complete (str pred flag)
"
Purpose:
   File name completion predicate.

Description:
   The function is an instance of a file name \"completion predicate\" required
   by the completing-read function. It is used by edstar-file-prompt when
   completing file names based on files accessible in directories on the
   EDSTAR_PATH search path.

Arguments:
   str
      The prefix string to be completed.
   pred
      Predicate to test suitability of a completion.
   flag
      Flag indicating what this function should do.

Notes:
   -  This function acceses the external symbols filelist and laststr, which
      should normally be initialised to nil before invoking completing-read,
      otherwise the list of files used may be out of date.
   -  See the completing-read documentation for more details of what this
      function does.
"

;;; If there is no current list of file names to use for completion, or there
;;; has been no previous string to complete, or the previously-completed
;;; string is not an abbreviation of the current one, then we need to get a
;;; new list of accessible files to use for completion.
   (if (or (not filelist) (not laststr)
           (not (string-match (concat "^" (regexp-quote laststr) ".*") str)))

;;; Get the set of file names which start with the current prefix string only
;;; (this saves time). Also record the prefix string.
       (progn
	 (setq filelist (edstar-possible-files str))
	 (setq laststr str)))

;;; If required, return the best completion of the string supplied.
   (cond ((not flag) (try-completion str filelist pred))

;;; If required, return a list of all possible completions.
         ((equal flag 't) (all-completions str filelist pred))

;;; If required, test for an exact match.
         ((equal flag 'lambda) (eq (try-completion str filelist pred) t))))
