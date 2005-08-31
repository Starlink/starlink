;;;###autoload
(defun edstar-split-path (path)
"
Purpose:
   Split a directory search path into fields.

Description:
   The function returns a list of strings obtained by splitting the contents of
   a \"path\" string  (which represents a list of directories to be searched)
   into its constituent elements. The \":\" character is recognised as the
   field delimiter within the path.

Arguments:
   path
      A string which holds the path specification to be split. If nil is
      given, the function returns nil.
"

;;; Define local variables.
  (let ((dirlist ())
        (start 0)
        (end))

;;; Return nil if no path string was given.
    (if (not path)
        nil

;;; Otherwise, scan through the path string.
      (while (< start (length path))
        (setq end start)

;;; Quit scanning if a ":" is found or the end of path is reached.
        (while
            (and (< end (length path))
                 (/= (elt path end) ?:))

;;; Increment to get the next character.
          (setq end (1+ end)))

;;; Extract each field which is found and cons it on to the start of a list.
        (setq dirlist (cons (substring path start end) dirlist))

;;; Increment start ready for the next field.
        (setq start (1+ end)))

;;; Return the reversed list.
      (reverse dirlist))))
