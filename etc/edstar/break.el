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
(defun starfort-backup-to-break ()
"
Purpose:
   Search backwards for a possible line break position.

Description:
   The function moves the current editing position (point) backwards on the
   current line until it locates a position which may be suitable for
   breaking the line. It returns the value of starfort-line-break-context
   at the resulting position.

Notes:
  -  The function searches backwards for a space which ends a word, or (if
  this would place it inside a quoted string) for the first character of a
  quoted word. If it cannot find a suitable position, point remains unchanged.
  -  This function should always leave point somewhere on the current line.
  -  This is a lower level routine and the position found must be further
  validated before use (e.g. by starfort-line-break-point) to protect against
  the possible creation of zero-length strings and to ensure that the required
  line length is not exceeded.
"

;;; Save the initial position and line break context.
  (let ((opoint (point))
        (context (starfort-line-break-context)))

;;; If we we are inside a quoted string, move backards until either white space
;;; or a quote is encountered. This will place us at the start of a word
;;; (if possible) while remaining within the quoted string.
    (if (equal context 'string)
	(skip-chars-backward "^ \t\n'")

;;; If we are not inside a string, skip backwards over any trailing white
;;; space.
      (skip-chars-backward " \t")

;;; If we are not at the end of a line or looking at white space, we are
;;; in the middle of a word, so skip backwards to find its start (this may
;;; move us into a string).
      (if (not (or (eolp) (looking-at "[ \t]")))
	  (skip-chars-backward "^ \t\n"))

;;; If we are now inside a string, then skip back to the start of a word.
      (setq context (starfort-line-break-context))
      (if (equal context 'string)
	  (skip-chars-backward "^ \t\n'")

;;; Otherwise, skip back to the end of a word.
	(skip-chars-backward " \t")))

;;; If we ended up at the start of the line, there is no perfect break point,
;;; so return to the initial position.
    (if (bolp) (goto-char opoint))

;;; Return the final line break context.
    context))

;;;###autoload
(defun starfort-line-break-point ()
"
Purpose:
   Find the best line break point on the current line.

Description:
   The function moves point to the best possible position for breaking the
   current line so as to produce a new line. It attempts to break at white
   space if possible and to make the resulting broken line as long as possible
   while not extending past (current-fill-column). It returns the value of the
   function starfort-line-break-context at the chosen position.
"

;;; Define locat variables.
  (let (ok context opoint)

;;; Move to the first possible line break position (just beyond the fill
;;; column).
    (move-to-column (1+ (current-fill-column)))

;;; Loop to test successively shorter maximum line lengths.
    (while (not ok)

;;; Remember the current starting position and move back to a possible break
;;; character.
      (setq opoint (point))
      (setq context (starfort-backup-to-break))

;;; Accept the position found if it is not part of a quoted string.
      (setq ok (or (not (equal context 'string))

;;; If it requires breaking a string, then check that there are sufficient
;;; columns remaining to insert the string continuation characters. Also
;;; check that it is not adjacent to a quote (which would create a zero-
;;; length string).
		   (and (<= (current-column) (- (current-fill-column) 3))
			(or (bolp) (/= (preceding-char) ?'))
			(or (eolp) (/= (following-char) ?')))))

;;; If this line break position is not acceptable, make the maximum line
;;; length one character shorter and try again.
      (if ok
	  ()
	(goto-char opoint)
	(backward-char)))

;;; Return the line break context.
    context))

;;;###autoload
(defun starfort-break-line (context)
"
Purpose:
   Break a line of Fortran at the current editing position.

Description:
   The function breaks a line of Fortran at the current editing position
   (point) to create a new line. If inside a comment line, a line of code or
   a quoted string (within code), then it is appropriately continued and
   indented on the new line.

Arguments:
   context
      The value of the function starfort-line-break-context at the point
      where the line is to be broken. This indicates what type of break
      (and continuation) is required.

Bugs:
   Full-line comment lines are not correctly indented when they are broken
   by this routine. More work needed here.
"

;;; If we are in an end of line comment, create a new comment line.
  (cond ((equal context 'eol-comment)
	 (indent-new-comment-line))

;;; If we are in a full-line comment, break it (adding a new comment start
;;; character) and indent the new line.
	((equal context 'line-comment)
	 (if (looking-at comment-line-start-skip)
	     (insert-before-markers comment-line-start "\n")
	   (delete-horizontal-space)
	   (insert-before-markers "\n" comment-line-start " "))
	 (starfort-indent-line))

;;; If we are at the start or end of a code line, simply start a new line,
;;; appropriately indented.
	((equal context 'code)
	 (delete-horizontal-space)
	 (insert-before-markers "\n      ")
	 (starfort-indent-line))

;;; If we are in a code line needing continuation, break it and insert the
;;; continuation character. Indent the new line.
	((equal context 'continue)
	 (delete-horizontal-space)
	 (insert-before-markers "\n     " starfort-continuation-char)
	 (starfort-indent-line))

;;; If we are in a quoted string, terminate it, break the line, create a
;;; continuation line and continue the string on the new line.
	((equal context 'string)
	 (insert-before-markers "' //\n     " starfort-continuation-char "'")
	 (starfort-indent-line))))

;;;###autoload
(defun starfort-do-auto-fill ()
"
Purpose:
   Perform auto-fill on Fortran lines.

Description:
   This function is intended for use by the auto-fill-hook in Starfort mode
   to perform automatic breaking of Fortran lines whenever a space character
   is entered beyond the (current-fill-column) (normally 71 in Starfort mode).
   It handles breaking of comment and code lines, including quoted strings,
   producing an appropriately indented continuation line.
"

;;; Check we are beyond the current fill column, otherwise there is
;;; nothing to do.
  (if (> (current-column) (current-fill-column))
      (progn

;;; Save the current buffer context.
	(save-excursion

;;; Move to the most suitable line break point, recording the context at that
;;; point.
	  (let ((context (starfort-line-break-point)))

;;; If the context is code, then check to see if there are any non-whitespace
;;; characters occurring earlier on the line (i.e. to see if we are positoned
;;; before the start of the statement). If not, then we must generate a
;;; statement continuation line.
	    (if (and (equal context 'code)
		     (not (save-excursion (skip-chars-backward " \t") (bolp))))
		(setq context 'continue))

;;; Break the line, generating the appropriate type of continuation line.
	    (starfort-break-line context)))

;;; We have now returned to the original editing position (on the new line).
;;; However, if there was no text on this line and indentation has been
;;; applied, point will now be positioned in front of the indentation. Move
;;; point to the new editing position.
	(starfort-indent-line))))

;;; Starfort line break function.
;;;###autoload
(defun starfort-breakline (&optional arg)
  (starfort-break-line (starfort-line-break-context)))
