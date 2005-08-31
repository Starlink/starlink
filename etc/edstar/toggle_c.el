;;;###autoload
(defun edstar-toggle-case ()
"
Purpose:
   Toggle the case (of a character or of the current selection).

Description:
   The function processes either the current character or, if a selection
   is active, all the text in the current selection. It toggles its case,
   changing lower case characters to upper case, and vice versa.

Notes:
   The current selection is cancelled by this function.
"
  (interactive)

;;; Define local variables.
  (let ((posn1)	(posn2) (str) (c) (l) (atstart))

;;; Save the current buffer context.
    (save-excursion

;;; Get the limits of the current selection (selects a single character after
;;; point if no selection is active).
      (setq posn1 (edstar-selection-beginning))
      (setq posn2 (edstar-selection-end))

;;; Note if point is positioned at the start of the selection.
      (setq atstart (eq (point) posn1))

;;; Extract the selected text into a string and determine its length.
      (setq str (buffer-substring posn1 posn2))
      (setq l (length str))

;;; Loop to process each character in the string.
      (setq i 0)
      (while (< i l)

;;; Extract the character from the string.
	(setq c (aref str i))

;;; If it is lower case, replace it with an upper case version.
	(if (and (>= c ?a) (<= c ?z))
	    (aset str i (upcase c))

;;; Otherwise, if it is upper case, replace it with a lower case version.
	  (if (and (>= c ?A) (<= c ?Z))
	      (aset str i (downcase c))))

;;; Increment to process the next character.
	(setq i (1+ i)))

;;; Go to the start of the text to be modified and delete the selected region.
        (goto-char posn1)
        (delete-region posn1 posn2)

;;; Replace it with the new string, inserting it after or before point as
;;; required.
        (if atstart (insert str)
	  (insert-before-markers str)))

;;; If no text was selected, then move one character in the required direction.
    (if (not (edstar-selection-isset))
	(if edstar-forward (forward-char) (backward-char)))

;;; Cancel the current selection (if any).
    (edstar-unset-selection)))
