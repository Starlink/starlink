;;;###autoload
(defun edstar-goto-eol ()
"
Purpose:
   Move to the end of a line.

Description:
   The function moves the editing position (point) to the next end of line,
   in either the forward or reverse direction (according to the current
   direction of motion).
"
  (interactive)

;;; Local variables.
  (let (start)

;;; Save the initial value of point.
    (setq start (point))

;;; If the current direction of motion is forward, move to the end of the
;;; current line (except if already at the end of a line, in which case
;;; go to the end of the following line).
    (if edstar-forward
	(progn
	  (if (eolp) (end-of-line 2) (end-of-line))

;;; If we failed to advance, we are at the end of the buffer, so report an
;;; error.
	  (if (<= (point) start)
	      (error "End of buffer")))

;;; If the current direction of motion is backwards, go to the end of the
;;; previous line.
      (end-of-line 0)

;;; If we failed to move backwards, we are at the beginning of the buffer, so
;;; report an error.
      (if (>= (point) start)
	  (error "Beginning of buffer")))))

;;;###autoload
(defun edstar-goto-page ()
"
Purpose:
   Move to the next page.

Description:
   The function moves the editing position (point) to the start of the
   next page of text, as marked by the next form-feed (^L) character in
   either the forward or reverse direction (according to the current
   direction of motion). If no form-feed characters are found, the editing
   position is moved to the appropriate end of the buffer.

Notes:
   The screen is scrolled as necessary to leave the start of the page near
   the top of the screen, if possible.
"
   (interactive)

;;; If the current direction of motion is forward, check we are not at the
;;; end of the buffer. Report an error if we are.
   (if edstar-forward
       (if (eobp)
	   (error "End of buffer")

;;; Skip over one character to avoid finding the same ^L for a second time.
	 (forward-char 1)

;;; Search for ^L. If found, move back over it. If not found, we end up at the
;;; end of the buffer.
	 (if (search-forward "\C-l" nil 1 nil) (backward-char 1)))

;;; If the current directon of motion is backwards, check we are not at the
;;; beginning of the buffer and report an error if necessary.
     (if (bobp)
	 (error "Beginning of buffer")

;;; Search mackwards for a ^L, ending up at the beginning of the buffer if
;;; it is not found.
       (search-backward "\C-l" nil 1 nil)))

;;; Move the point found to the top of the screen (scrolling will subsequently
;;; re-position this to a sensible point near the top).
   (recenter 0))

;;;###autoload
(defun edstar-goto-word-forward ()
"
Purpose:
   Move forward to the next word.

Description:
   The function moves the editing position (point) forward to the start of
   the next word (or other non-word character which is not white space).
"
   (interactive)

;;; Skip one character so we do not find a word starting at the current
;;; position.
   (forward-char 1)

;;; Search forward for the start of a word or any other type of non-word
;;; character which is not classified as white space.
   (if (re-search-forward (concat "\\<\\|"
				  "\\(\\s_\\|\\s.\\|\\s(\\|\\s)\\|\\s\"\\|"
				  "\\s\$\\|\\s\\\\|\\s/\\|\\s'\\|"
				  "\\s<\\|\\s>\\)")
			  nil t nil)

;;; If found, go to the required position.
       (goto-char (match-beginning 0))

;;; Otherwise, go to the end of the buffer.
     (goto-char (point-max))))

;;;###autoload
(defun edstar-goto-word-reverse ()
"
Purpose:
   Move backwards to the previous word.

Description:
   The function moves the editing position (point) backwards to the start of
   the previous word (or other non-word character which is not white space).

Notes:
   This function simulates the similar motion command provided by STARLSE.

Bugs:
   Non-word (e.g. punctuation) characters are sometimes missed by this
   function if the editing position is initially positioned immediately
   following them. The regular expression matching behaviour of
   the re-search-backward function needs more investigation in this area.
"
   (interactive)

;;; Skip one character so we do not find a word starting at the current
;;; position.
   (backward-char 1)

;;; Search backward for the start of a word or any other type of non-word
;;; character which is not classified as white space.
   (if (re-search-backward (concat "\\<\\|"
				   "\\(\\s_\\|\\s.\\|\\s(\\|\\s)\\|\\s\"\\|"
				   "\\s\$\\|\\s\\\\|\\s/\\|\\s'\\|"
				   "\\s<\\|\\s>\\)")
			   nil t nil)

;;; If found, go to the required position.
       (goto-char (match-beginning 0))

;;; Otherwise, go to the beginning of the buffer.
     (goto-char (point-min))))

;;;###autoload
(defun edstar-goto-word ()
"
Purpose:
   Move to the next word.

Description:
   The function moves the editing position (point) either forwards or backwards
   to the start of the next (or previous) word, or to any other non-word
   character which is not white space. The default direction of motion is used.

Notes:
   This function simulates the similar motion command provided by STARLSE.
"
  (interactive)

;;; Move forward or in reverse, as required.
  (if edstar-forward (edstar-goto-word-forward)
    (edstar-goto-word-reverse)))

;;;###autoload
(defun edstar-goto-top ()
"
Purpose:
   Move to the top of the buffer.

Description:
   The function moves the editing position (point) to the top of the
   buffer.
"
  (interactive)

;;; Go to the top of the buffer.
  (goto-char (point-min)))

;;;###autoload
(defun edstar-goto-bottom ()
"
Purpose:
   Move to the bottom of the buffer.

Description:
   The function moves the editing position (point) to the bottom of the
   buffer.
"
  (interactive)

;;; Go to the end of the buffer.
  (goto-char (point-max)))

;;;###autoload
(defun edstar-scroll-forward ()
"
Purpose:
   Scroll towards the end of the buffer by about one window height.

Description:
   The function moves the editing position (point) towards the bottom of the
   buffer and positions it at the start of a line. The amount of movement is
   designed to display the next screenful of buffer text.

Notes:
   The number of lines moved is equal to the number of lines available for
   displaying text in the window, less the value \"next-screen-context-lines\",
   which provides some continuity between screens.
"
  (interactive)

;;; Find which window line point is currently in.
  (let ((line (edstar-current-line-in-window)))

;;; Move down the appropriate number of lines.
    (vertical-motion (- (window-height) 1 next-screen-context-lines))

;;; Scroll the window to restore point to its original windw line.
    (recenter line)))

;;;###autoload
(defun edstar-scroll-reverse ()
"
Purpose:
   Scroll towards the beginning of the buffer by about one window height.

Description:
   The function moves the editing position (point) towards the beginning of the
   buffer and positions it at the start of a line. The amount of movement is
   designed to display the previous screenful of buffer text.

Notes:
   The number of lines moved is equal to the number of lines available for
   displaying text in the window, less the value \"next-screen-context-lines\",
   which provides some continuity between screens.
"
  (interactive)

;;; Find which window line point is currently in.
  (let ((line (edstar-current-line-in-window)))

;;; Move up the appropriate number of lines.
    (vertical-motion (- (- (window-height) 1 next-screen-context-lines)))

;;; Scroll the window to restore point to its original windw line.
    (recenter line)))

;;;###autoload
(defun edstar-scroll ()
"
Purpose:
   Scroll the buffer by about one window height.

Description:
   The function moves the editing position (point) towards the beginning or
   end of the buffer and positions it at the start of a line. The amount of
   movement is designed to display the adjacent screenful of buffer text. The
   direction of scrolling is determined by the current buffer direction
   setting (forward/reverse).

Notes:
   The number of lines moved is equal to the number of lines available for
   displaying text in the window, less the value \"next-screen-context-lines\",
   which provides some continuity between screens.
"
  (interactive)

;;; Scroll the screen in the direction required.
  (if edstar-forward
      (edstar-scroll-forward)
    (edstar-scroll-reverse)))
