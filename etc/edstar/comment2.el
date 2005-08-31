;;;###autoload
(defun starfort-line-break-context ()
"
Purpose:
   Determine how a line should be broken at the current position.

Description:
   The function returns a symbol indicating the context of the current editing
   position (point), indicating how the current line should be broken at that
   point so as to create a new line. It returns one of the following symbols:

     line-comment --> Point is in a full-line comment line.
     eol-comment  --> Point is inside an end-of-line (!) comment.
     code         --> Point is at the start or end of a line of code.
     continuation --> Point is inside a line of code, which must be continued.
     string       --> Point is inside a quoted string within a line of code.

Bugs:
   This function takes no account of code lines which are continued, so quoted
   strings should not be split across continuation lines.
"

;;; Define local variables, saving the initial value of point.
  (let ((opoint (point)) quoted comment text continue eol)

;;; Save the current buffer context and move to the start of the current line.
    (save-excursion
      (beginning-of-line)

;;; Note if we are looking at the start of a full-line comment.
      (if (looking-at comment-line-start-skip) 'line-comment

;;; Otherwise, loop to inspect each character between the beginning of the
;;; line and the original value of point, looking for an end-of-line comment
;;; character.
	(while (and (< (point) opoint) (not comment))

;;; Take account of quotes. Note if an unquoted comment character is found.
;;; Also note if any non-whitespace characters are found.
	  (if (looking-at "'") (setq quoted (not quoted)))
	  (if (and (not quoted) (looking-at "!")) (setq comment t))
	  (if (not (looking-at "[ \t]")) (setq text t))

;;; Increment to inspect the next character.
	  (forward-char))

;;; If the above loop exited while inside quotes, we are in a quoted string.
	(if quoted 'string

;;; If an unquoted comment character was found, we are in an end of line
;;; comment.
	  (if comment 'eol-comment

;;; If no non-whitespace characters were found, we are at the start of a code
;;; line.
	    (if (not text) 'code

;;; Otherwise, we must determine if we are at the end of a code line (or within
;;; it). Obtain the position of the end of the line.
	      (setq eol (save-excursion (end-of-line) (point)))

;;; Loop to inspect each character up to the end of the line. Quit if an
;;; end of line comment character was found or if other any non-whitespace
;;; character is found first.
	      (while (and (< (point) eol) (not continue) (not comment))
		(setq comment (looking-at "!"))
		(if (not comment) (setq continue (not (looking-at "[ \t]"))))
		(forward-char))

;;; If further non-whitespace characters were found, we are inside a code
;;; line which must be continued. Otherwise, we are at the end of a code line.
	      (if continue 'continue
		'code))))))))

;;;###autoload
(defun starfort-break-line ()
"
Purpose:
   Break a line of Fortran at the current editing position.

Description:
   The function breaks a line of Fortran at the current editing position
   (point) to create a new line. If inside a comment line, a line of code or
   a quoted string (within code), then it is appropriately continued and
   indented on the new line.

Bugs:
   Full-line comment lines are not correctly indented when they are broken
   by this routine. More work needed here.
"
  (interactive)

;;; Define local variables. Obtain the context of the line break operation.
  (let ((type (starfort-line-break-context)))

;;; If we are in an end of line comment, create a new comment line.
    (cond ((equal type 'eol-comment) (indent-new-comment-line))

;;; If we are in a full-line comment, break it (adding a new comment start
;;; character) and indent the new line.
	  ((equal type 'line-comment)
	   (if (looking-at comment-line-start-skip)
	       (insert comment-line-start "\n")
	     (insert "\n" comment-line-start " "))
	   (starfort-indent-line))

;;; If we are at the start or end of a code line, simply start a new line,
;;; appropriately indented.
	  ((equal type 'code)
	   (insert "\n      ")
	   (starfort-indent-line))

;;; If we are in a code line needing continuation, break it and insert the
;;; continuation character. Indent the new line.
	  ((equal type 'continue)
	   (insert "\n     " starfort-continuation-char)
	   (starfort-indent-line))

;;; If we are in a quoted string, terminate it, break the line, create a
;;; continuation line and continue the string on the new line.
	  ((equal type 'string)
	   (insert "' //\n     " starfort-continuation-char "'")
	   (starfort-indent-line)))))
