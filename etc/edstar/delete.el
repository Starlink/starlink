;;;###autoload (defvar edstar-char-was-deleted-forward t
;;;###autoload "
;;;###autoload    Stores whether the last character deleted was deleted in the forward
;;;###autoload    direction or not. This determines whether it is inserted after or before
;;;###autoload    point when it is pasted back.
;;;###autoload ")

;;;###autoload
(defun edstar-delete-char ()
"
Purpose:
   Delete the next character.

Description:
   The function deletes the character which follows the current editing
   position (point). The deleted text is stored in the \"c\" register and
   may subsequently be pasted back by using \"edstar-undelete-char\".
"
  (interactive)

;;; Check that we are not at the end of the buffer. Report an error if we are.
  (if (eobp)
      (error "End of buffer")

;;; Copy the text between point and the following character to the "c"
;;; register.
    (copy-to-register ?c (point) (1+ (point)) t)

;;; Record that the character was deleted in the forward direction.
    (setq edstar-char-was-deleted-forward t)))

;;;###autoload
(defun edstar-delete-char-reverse ()
"
Purpose:
   Delete the previous character.

Description:
   The function deletes the character which precedes the current editing
   position (point). The deleted text is stored in the \"c\" register and
   may subsequently be pasted back by using \"edstar-undelete-char\".

Notes:
   If the preceding character is a tab, it is first converted to an
   equivalent number of spaces to preserve alignment. The final resulting
   space is then deleted.
"
 (interactive)

;;; Check that we are not at the beginning of the buffer. Report an error if
;;; we are.
 (if (bobp)
     (error "Beginning of buffer")

;;; See if the preceding character is a tab. If so, delete one character
;;; backwards with un-tabification.
   (if (= (preceding-char) ?\t)
       (progn
	 (backward-delete-char-untabify 1)

;;; Restore the deleted space, since we need to save it.
	 (insert " ")))

;;; Copy the character before point to the "c" register.
   (copy-to-register ?c (- (point) 1) (point) t)

;;; Record that the character was deleted in the reverse direction.
   (setq edstar-char-was-deleted-forward nil)))

;;;###autoload
(defun edstar-undelete-char ()
"
Purpose:
   Paste back the text deleted by \"edstar-delete-char\" (or
   \"edstar-delete-char-reverse\").

Description:
   The function pastes back the text last deleted by \"edstar-delete-char\"
   or \"edstar-delete-char-reverse\". The restored text is inserted immediately
   after the current editing position (point) if it was deleted in the
   forward direction, otherwise it is inserted immediately in front of the
   editing position.

Notes:
   The deleted text is recovered from the \"c\" register.
"
  (interactive)

;;; Insert the contents of the "c" register after or before point, as
;;; required.
  (insert-register ?c (not edstar-char-was-deleted-forward)))

;;;###autoload
(defun edstar-delete-word ()
"
Purpose:
   Delete text up to the next word.

Description:
   The function deletes the text between the current editing position (point)
   and the start of the following word (the criteria defining a word are the
   same as used by \"edstar-goto-word\", except that motion is always in the
   forward direction). The deleted text is stored in the \"w\" register and
   may subsequently be pasted back by using \"edstar-undelete-word\".
"
  (interactive)

;;; Copy the text between point and the start of the next word to the "w"
;;; register.
  (copy-to-register
   ?w (point) (save-excursion
		(edstar-goto-word-forward)
		(point)) t))

;;;###autoload
(defun edstar-undelete-word ()
"
Purpose:
   Paste back text deleted by \"edstar-delete-word\".

Description:
   The function pastes back the text last deleted by \"edstar-delete-word\",
   inserting it immediately after the current editing position (point).

Notes:
   The deleted text is recovered from the \"w\" register.
"
  (interactive)

;;; Insert the contents of the "w" register after point.
  (insert-register ?w nil))

;;;###autoload
(defun edstar-delete-line ()
"
Purpose:
   Delete a line.

Description:
   The function deletes the text between the current editing position (point)
   and the start of the next line. The deleted text is stored in the \"l\"
   register and may subsequently be pasted back by using
   \"edstar-undelete-line\".
"
  (interactive)

;;; Check if we are at the end of the buffer. Report an error if we are.
  (if (eobp)
      (error "End of buffer")

;;; Copy the text between point and the start of the next line to the "l"
;;; register.
    (copy-to-register
     ?l (point) (save-excursion
		  (forward-line 1) (point)) t)))

;;;###autoload
(defun edstar-delete-eol ()
"
Purpose:
   Delete to the end of the line.

Description:
   The function deletes the text between the current editing position (point)
   and the end of the current line. The deleted text is stored in the \"l\"
   register and may subsequently be pasted back by using
   \"edstar-undelete-line\".

Notes:
   If the editing position is at the end of a line, text is deleted up to
   the end of the following line instead.
"
  (interactive)

;;; Check if we are at the end of the buffer. Report an error if we are.
  (if (eobp)
      (error "End of buffer")

;;; If we are at the end of a line, copy the text between point and the end
;;; of the next line to the "l" register.
    (if (eolp)
	(copy-to-register
	 ?l (point) (save-excursion
		      (end-of-line 2) (point)) t)

;;; Otherwise, copy the text between point and the end of the current line.
      (copy-to-register
       ?l (point) (save-excursion
		    (end-of-line 1) (point)) t))))

;;;###autoload
(defun edstar-undelete-line ()
"
Purpose:
   Paste back text deleted by \"edstar-delete-line\" (or
   \"edstar-delete-eol\").

Description:
   The function pastes back the text last deleted by \"edstar-delete-line\"
   or \"edstar-delete-eol\", inserting it immediately after the curren
   editing position (point).

Notes:
   The deleted text is recovered from the \"l\" register.
"
  (interactive)

;;; Insert the contents of the "l" register after point.
  (insert-register ?l nil))
