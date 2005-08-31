;;;###autoload
(defun edstar-cut ()
"
Purpose:
   Cut out (delete) the selected text, storing it in the paste register.

Description:
   The function cuts the currently-selected text out of the current buffer
   (thus deleting it) and places it into the paste register (register \"p\"). It
   may later be pasted back elsewhere by using the \"edstar-paste\" function.

Notes:
   -  An error results if no text is selected.
   -  This function cancels the current selection.
"
  (interactive)

;;; Check that there is a current selection. Report an error if there is not.
  (if (not (edstar-selection-isset))
      (error "No selection active")

;;; Copy the selected text to the "p" register.
    (copy-to-register ?p
		      (edstar-selection-beginning)
		      (edstar-selection-end) t)

;;; Cancel the current selection.
    (edstar-unset-selection)))

;;;###autoload
(defun edstar-cut-append ()
"
Purpose:
   Cut out (delete) the selected text, appending it to the paste register.

Description:
   The function cuts the currently-selected text out of the current buffer
   (thus deleting it) and appends it to the paste register (register \"p\"). It
   may later be pasted back elsewhere by using the \"edstar-paste\" function.

Notes:
   -  An error results if no text is selected.
   -  This function cancels the current selection.
"
  (interactive)

;;; Check that there is a current selection. Report an error if there is not.
  (if (not (edstar-selection-isset))
      (error "No selection active")

;;; Append the selected text to the "p" register.
    (append-to-register ?p
			(edstar-selection-beginning)
			(edstar-selection-end) t)

;;; Cancel the current selection.
    (edstar-unset-selection)))

;;;###autoload
(defun edstar-paste ()
"
Purpose:
   Paste previously-cut text back into the current buffer.

Description:
   The function pastes back text which has previously been cut (using the
   function \"edstar-cut\") and stored in the paste register (register \"p\").
   The text is interted immediately before the current editing position
   (point).
"
  (interactive)

;;; Insert the paste register contents before point.
  (insert-register ?p t))

;;;###autoload
(defun edstar-cut-replace ()
"
Purpose:
   Cut the selected text, replacing it with the paste register contents.

Description:
   The function cuts the currently-selected text from the current buffer
   and replaces it with the contents of the paste register (register \"p\").
   The text which is cut out is stored in register \"s\".

Notes:
   -  An error results if no text is selected.
   -  This function cancels the current selection.
"
  (interactive)

;;; Check that there is a current selection. Report an error if there is not.
  (if (not (edstar-selection-isset))
      (error "No selection active")

;;; Copy the selected text to the "s" register.
    (copy-to-register ?s
		      (edstar-selection-beginning)
		      (edstar-selection-end) t)

;;; Insert the contents of register p before point.
    (insert-register ?p t)

;;; Cancel the current selection.
    (edstar-unset-selection)))
