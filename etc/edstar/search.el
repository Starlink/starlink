;;; Define variable to hold last search string.
;;;###autoload (defvar edstar-search-target "")

;;;###autoload
(defun edstar-search (&optional target)
"
Purpose:
   Search for a string.

Description:
   The function searches for a specified string in the current buffer, starting
   at the current editing position (point) and searching forwards or backwards
   according to the current default direction setting. If the string is found,
   the editing position (point) is moved to the start of it. Otherwise, an
   error message is given.

Arguments
   target (optional)
      The string to be searched for. This is prompted for if not given.

Notes:
   This function saves the target string so that it may be searched for again
   (e.g. using edstar-search-again).
"
  (interactive)

;;; Define local variables.
  (let (found)

;;; If the target string was not given, then prompt for it, indicating the
;;; default value in the prompt string.
    (if (not target)
	(setq target
	      (read-from-minibuffer
	       (if (string= edstar-search-target "")
		   "Search for: "
		 (concat "Search for [" edstar-search-target "]: ")))))

;;; If a non-blank target was given, save it. Otherwise continue with the
;;; previous target string.
    (if (not (string= target "")) (setq edstar-search-target target))

;;; Record the current buffer status.
    (save-excursion

;;; If searching forward, move over the next character (to prevent finding
;;; a string starting at the current position) and search.
      (if edstar-forward
	  (progn
	    (if (< (point) (point-max)) (forward-char 1))
	    (setq found (search-forward edstar-search-target nil t nil)))

;;; If searching backwards, we can search directly.
	(setq found (search-backward edstar-search-target nil t nil))))

;;; If the string was found, move to its beginning.
    (if found
	(goto-char (match-beginning 0))

;;; Otherwise display an error message.
    (message (concat "Could not find: " edstar-search-target)))

;;; Don't deactivate the current selection (if set).
    (setq deactivate-mark nil)))

;;;###autoload
(defun edstar-search-again ()
"
Purpose:
   Repeat a search for a string.

Description:
   The function repeats a search for a string previously searched for using
   edstar-search. It performs the same search operations as that function,
   except that a search string is not given; the previous one is used again.
"
  (interactive)

;;; Perform a search using the prevous search string.
  (edstar-search edstar-search-target))
