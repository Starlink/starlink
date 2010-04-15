(provide 'newline)

;;; Save the "native" emacs definition of newline.
(fset 'edstar-native-newline (symbol-function 'newline))

;;; Turn newline into a virtual function.
(edstar-make-virtual newline (&optional arg)
"
Purpose:
   Break the current line to create a new line.

Description:
   The function breaks a line of Fortran into two so as to create a new line.
   Comment lines, lines of code and quoted strings (within code) are
   appropriately continued and indented on the new line. If the editing
   position (point) is not initially beyond the buffer's fill column, the line
   is broken at the editing position. Otherwise, it is broken at the most
   suitable point which does not leave the first portion of the line extending
   beyond the fill column.

Notes:
   Placeholders will not be split by this function, even if the editing
   position lies within one.
"
  (interactive))

;;; Globally override the default implementation of newline so that it does
;;; not split placeholders.
(edstar-override newline (&optional arg)

;;; Define local variables
  (let (place)

;;; Check if we are currently inside a placeholder (this moves us on to the
;;; first character of its name if we are).
    (if (setq place (edstar-in-place))

;;; If so, save the current buffer context and move to the start of the
;;; placeholder, so it will not be split.
	(save-excursion
	  (goto-char (cdr (assq 'beginning place)))

;;; If point is now not beyond the fill column, or there is no auto fill
;;; function, simply break the line.
	  (if (or (<= (current-column) (current-fill-column))
                  (not auto-fill-function))
	      (edstar-breakline)

;;; Otherwise use the auto fill function to choose the break point.
	    (funcall auto-fill-function)))

;;; If we are not within a placeholder and point is not beyond the fill
;;; column, simply break the line.
      (if (or (<= (current-column) (current-fill-column))
              (not auto-fill-function))
	  (edstar-breakline)

;;; Otherwise use the auto fill function to select a suitable break point.
	(funcall auto-fill-function)))))
