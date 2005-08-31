;;;###autoload
(defun edstar-set-selection ()
"
Purpose:
   Set the start of the text selection.

Description:
   The function sets the start of the current text selection to the current
   editing position (point) in the current buffer, overriding any previous
   value. This makes the text selection active, the selected text being that
   lying between the nominated start position and any subsequent editing
   position (point).

Notes:
   -  This function sets the mark for the current buffer to the value of point
   and pushes the previous mark value (if any) on to the mark ring. The status
   of the mark (active or inactive) is subsequently used to identify whether
   a text selection is active.
   -  This function requires \"transient mark mode\" to be set for correct
   operation (which includes highlighting of selected text). It will set this
   mode itself if necessary.
"
   (interactive)

;;; Ensure that transient mark mode is enabled, to allow highlighting of
;;; the region.
   (setq transient-mark-mode 1)

;;; Set the mark.
   (push-mark nil t t))

;;;###autoload
(defun edstar-unset-selection ()
"
Purpose:
   Unset the current text selection.

Description:
   The function clears the current text selection, de-selecting any selected
   text.

Notes:
   This function makes the value of mark inactive in the current buffer to
   indicate that there is no current text selection.
"
   (interactive)

;;; If there is no current selection, report an error.
   (if (not mark-active)
       (error "No selection active")

;;; Request that the mark be made inactive.
     (setq deactivate-mark t)))

;;;###autoload
(defun edstar-selection-beginning ()
"
Purpose:
   Return the beginning of the current text selection.

Description:
   If text is currently selected, the position of the beginning of the
   selection is returned. Otherwise, the current editing position (point) is
   returned. The returned position will not lie outside the accessible region
   of the current buffer.
"

;;; Constrain the returned result to the start of the accessible region of
;;; the buffer.
      (max (point-min)

;;; If mark is active, return the beginning position of the selection/region.
	   (if mark-active
	       (min (mark) (point))

;;; Otherwise, return point.
	     (point))))

;;;###autoload
(defun edstar-selection-end ()
"
Purpose:
   Return the end of the current selection.

Description:
   If text is currently selected, the position of the end of the selection is
   returned. Otherwise, a position one character beyond the current editing
   position (point) is returned. The returned position will not lie outside
   the accessible region of the current buffer.
"

;;; Constrain the returned result to the end of the accessible region of
;;; the buffer.
      (min (point-max)

;;; If mark is active, return the beginning position of the selection/region.
	   (if mark-active
	       (max (mark) (point))

;;; Otherwise, return a position one character beyond point.
	     (1+ (point)))))

;;;###autoload
(defun edstar-selection-isset ()
"
Purpose:
   Determine if text is currently selected.

Description:
   The function returns t if there is currently selected text, nil otherwise.

Notes:
   The status of the mark (active/inactive) is used to determine if a
   selection is set.
"

;;; Test the mark.
   (if mark-active t nil))
