;;;###autoload
(defun edstar-handle-menu-motion ()
"
Purpose:
   Control the motion of the cursor and window contents in a menu.

Description:
   This function is called by the functions invoked to perform cursor motion
   in a menu window created by edstar-select-from-menu. It moves the arrow
   cursor used to select from the menu (by re-drawing it). It also constrains
   the normal cursor motion to the first column and ensures that the menu
   window does not scroll beyond the beginning or end of the buffer which
   contains the menu options.

Returned Value:
   nil

Notes:
   The menu buffer should be current and displayed in a window before calling
   this function.

Global Variables:
   oldpos
      This is shared with edstar-select-from-window and records the previous
      position of the selection cursor.
"

;;; Make the menu buffer writeable.
  (setq buffer-read-only nil)

;;; Go to the previously-selected buffer position and erase the arrow cursor,
;;; replacing it with spaces.
  (save-excursion
    (goto-char oldpos)
    (delete-char 3)
    (insert "   "))

;;; Constrain motion within the buffer to the first column only.
  (beginning-of-line)

;;; Insert an arrow cursor on the newly-selected line.
  (delete-char 3)
  (insert "-->")
  (beginning-of-line)

;;; Make the buffer read-only again and record the currently selected line.
  (setq buffer-read-only t)
  (setq oldpos (point))

;;; Now limit the vertical scrolling of the window.
;;; ----------------------------------------------
;;; If the first menu line is visible in the window, then ensure it is at the
;;; top.
  (if (pos-visible-in-window-p (point-min))

;;; Do this by moving down by the number of lines visible on the screen and
;;; ensuring that the resulting position is also visible. If so, there is
;;; nothing to do.
      (save-excursion
	(goto-char (point-min))
	(forward-line (- (window-height) 2))
	(if (pos-visible-in-window-p)
	    ()

;;; Otherwise, re-position the first menu line at the top of the screen.
	  (goto-char (point-min))
	  (recenter 0)))

;;; If the first menu line is not visible, then check the last line.
    (if (pos-visible-in-window-p (point-max))

;;; If the last line is visible, move backwards by the number of lines visible
;;; on the screen and check that the resulting position is also visible.
	(save-excursion
	  (goto-char (point-max))
	  (forward-line (- 2 (window-height)))
	  (if (pos-visible-in-window-p)
	      ()

;;; If necessary, reposition the last menu line at the bottom of the screen.
	    (goto-char (point-max))
	    (recenter -1)))))

;;; Return nil.
  nil)
