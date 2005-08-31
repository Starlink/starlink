;;;###autoload
(defun edstar-abs-file (file)
"
Purpose:
   Determine if a file name is absolute.

Description:
   The function returns non-nil if the string passed as an argument is an
   absolute file name, in the sense that it should not be searched for using
   any directory path. This will be the case if it contains a \"/\" character
   anywhere within it. (Note that relative file names containing \"/\" are
   considered \"absolute\" by this function.)
"
;;; Define local variables.
  (let ((i 0)
	(found nil))

;;; Scan the string supplied, quitting if a "/" is found or the end of string
;;; is reached.
    (while (and (< i (length file))
		(not found))

;;; Note if the current character is a "/".
      (setq found (= ?/ (elt file i)))

;;; Increment to test the next character.
      (setq i (1+ i)))

;;; Return whether "/" was found.
    found))
