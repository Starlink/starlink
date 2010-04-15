;;; Placeholder delimiters.
;;;###autoload (defvar edstar-placeholder-begin "{")
;;;###autoload (defvar edstar-placeholder-end "}")
;;;###autoload (defvar edstar-placeholder-begin-opt "[")
;;;###autoload (defvar edstar-placeholder-end-opt "]")
;;;###autoload (defvar edstar-placeholder-dupe "...")

;;; Expression to match placeholder contents (i.e. name). Note that "-" is not
;;; correctly quoted by regexp-quote in this context, so must be escaped
;;; separately at the end. (NB must all be on one line for autoload to work.)
;;;###autoload (defvar edstar-placeholder-chars (concat "[" (regexp-quote " A-Za-z0-9~`@#$%^&*()_+=|\\:;\"'<,>.?/\\-") "]"))

;;; Moves to next placeholder string in requested direction, returning alist of
;;; placeholder attributes. Returns nil if none found.
;;;###autoload
(defun edstar-goto-place-string (forward &optional limit noerror)
  (interactive)

;;; Define local variables.
  (let (place found n1 n2 opt dupe)

;;; Set up a regular expression to match a placeholder.
    (setq place
	  (concat "\\("
		  (regexp-quote edstar-placeholder-begin)
		  "\\(\\(" edstar-placeholder-chars "\\)+\\)"
		  (regexp-quote edstar-placeholder-end)
		  "\\|"
		  (regexp-quote edstar-placeholder-begin-opt)
		  "\\(\\(" edstar-placeholder-chars "\\)+\\)"
		  (regexp-quote edstar-placeholder-end-opt)
		  "\\)\\("
		  (regexp-quote edstar-placeholder-dupe)
		  "\\|\\)"))

;;; Save the current buffer status and position of point and search for the
;;; required placeholder, noting if found.
    (let ((opoint (point)))
      (save-excursion
	(setq found

;;; If searching forward, first check if we are already looking at a
;;; placeholder and step over it if necessary, to save finding it again.
	      (if forward
		  (progn
		    (if (looking-at place) (forward-char))
		    (re-search-forward place limit t nil))

;;; If searching backwards and a placeholder is found, it is possible that the
;;; optional trailing "duplicate" string may have been missed if it extends
;;; past the initial position of point.
		(if (re-search-backward place limit t nil)
		    (progn

;;; To check for this, search again in the forwards direction and check that
;;; the string matched does not extend too far. If it does, search again for an
;;; earlier one.
		      (re-search-forward place nil t nil)
		      (goto-char (match-beginning 0))
		      (if (> (match-end 0) opoint)
			  (re-search-backward place limit t nil)
			t)))))))

;;; Return nil if no placeholder was found.
    (if (not found)
	(progn
	  (if (not noerror) (message "No more placeholders"))
	  nil)

;;; Otherwise, determine which part of the expression was matched and obtain
;;; the positions of the placeholder contents (its name) accordingly.
      (setq opt (not (match-beginning 2)))
      (setq n1 (if opt (match-beginning 4) (match-beginning 2)))
      (setq n2 (if opt (match-end 4) (match-end 2)))

;;; Note if the placeholder is to be duplicated (i.e. has the "duplicate"
;;; string appended).
      (setq dupe (< (match-beginning 6) (match-end 6)))

;;; Move to the first character in its name.
      (goto-char n1)

;;; Return an alist associating the placeholder attributes with appropriate
;;; symbols which may later be used to retrieve these values.
      (list (cons 'beginning (match-beginning 0)) ; First character
	    (cons 'end (match-end 0))             ; Last character
	    (cons 'name-beginning n1)             ; First character in name
	    (cons 'name-end n2)                   ; Last character in name
	    (cons 'name (buffer-substring n1 n2)) ; Placeholder name string
	    (cons 'opt opt)                       ; Placeholder is optional?
	    (cons 'dupe dupe)))))                 ; Duplicate the placeholder?

;;; Goto placeholder, validating its name and returning its value and
;;; property list.
;;;###autoload
(defun edstar-goto-place (forward &optional limit noerror)

;;; Define local variables.
  (let (found place posn)

;;; Save the current buffer context while we search for a placeholder.
    (save-excursion

;;; Continue looking while a placeholder has not been found and further
;;; placeholder-like strings still exist to be checked.
      (while (and (not found)
		  (setq place
			(edstar-goto-place-string forward limit noerror)))

;;; If a possible placeholder has been found, look up its name to check if
;;; it is valid.
	(if (setq found (edstar-lookup-place (cdr (assq 'name place))))

;;; If so, extract its translation value and list of properties and add
;;; them to the placeholder information alist.
	    (progn
	      (setq posn (point))
	      (setq place (append
			   (list (cons 'value (symbol-value found))
				 (cons 'plist (symbol-plist found)))
			   place))))))

;;; If no placeholder was found, return nil.
    (if (not found)
	nil

;;; Otherwise, reposition to the placeholder and return the associated
;;; placeholder information.
      (goto-char posn)
      place)))

;;; Determine if we are inside a placeholder, moving to the start of its
;;; name if we are.
;;;###autoload
(defun edstar-in-place ()
  (interactive)

;;; Define local variables.
  (let (opoint limit place beginning end opt)

;;; Save the initial value of point and the buffer context.
    (setq opoint (point))
    (save-excursion

;;; To save time, we need only inspect the current line for placeholders
;;; (they cannot span lines), so obtain the position of the end of the current
;;; line.
      (save-excursion
	(end-of-line)
	(setq limit (point)))

;;; Since we cannot search for a placeholder if we are already positioned
;;; inside it, we must first move off it by going forwards. However, we must
;;; avoid moving over any later ones on the same line, or we may locate them
;;; by mistake. Search forwards until we hit a preceding placeholder or
;;; the end of the line. Position at the end of the line if no
;;; placeholders were found. We know the end-of-line cannot form part of
;;; a placeholder.
      (if (not (edstar-goto-place t limit t))
	  (goto-char limit))

;;; Find the position of the beginning of the current line.
      (save-excursion
	(beginning-of-line)
	(setq limit (point)))

;;; Search backwards for the previous placeholder on the current line.
      (if (setq place (edstar-goto-place nil limit t))

;;; If one was found, then extract its beginning and end positions.
	  (progn
	    (setq beginning (cdr (assq 'beginning place)))
	    (setq end (cdr (assq 'end place)))

;;; Check if the initial value of point lies inside the placeholder (taking
;;; account of the delimiters at either end). Ignore the placeholder if it does
;;; not.
	    (if (or (< opoint beginning) (>= opoint end))
		(setq place nil)))))

;;; If we are inside a placeholder, reposition to the first character of its
;;; name.
    (if place (goto-char (cdr (assq 'name-beginning place))))

;;; Return the placeholder information.
    place))

;;; Finds nearest placeholder and positions on it.
;;;###autoload
(defun edstar-best-place (forward &optional limit noerror)
  (interactive)
  (let (place)
    (if (not (setq place (edstar-in-place)))
	(if (not (setq place (edstar-goto-place forward limit t)))
	    (if (not noerror) (message "No nearby placeholder found"))))
    place))

;;;###autoload
(defun edstar-kill-place ()
  (interactive)
  (let (place ok beginning end empty sep head tail opoint seperase)
    (edstar-expand-auto-place)
    (if (setq place (edstar-best-place edstar-forward))
	(progn
	  (setq ok (cdr (assq 'opt place)))
	  (if (not ok)
	      (setq ok
		    (y-or-n-p
		     (concat "Placeholder "
			     edstar-placeholder-begin (cdr (assq 'name place)) edstar-placeholder-end
			     " is not optional. Delete anyway? "))))
	  (if ok
	      (progn
		(setq beginning (cdr (assq 'beginning place)))
		(setq end (cdr (assq 'end place)))
		(goto-char beginning)
		(delete-region beginning end)

		(setq plist (cdr (assq 'plist place)))
		(setq sep (cdr (assq 'sep plist)))
		(setq head (cdr (assq 'head plist)))
		(setq tail (cdr (assq 'tail plist)))
                (setq prehead (cdr (assq 'prehead plist)))
                (setq posthead (cdr (assq 'posthead plist)))
                (setq pretail (cdr (assq 'pretail plist)))
                (setq posttail (cdr (assq 'posttail plist)))
		(if sep
		    (save-excursion
		      (setq opoint (point))
		      (backward-char (length sep))
		      (if (setq seperase (looking-at (regexp-quote sep)))
			  (progn
			    (skip-chars-backward " \t")
			    (delete-region (point) opoint)))))
		(if (not seperase)
		    (progn
		      (if head
			  (save-excursion
                            (if posthead (skip-chars-backward " \t"))
			    (setq opoint (point))
			    (if (not posthead) (skip-chars-backward " \t"))
			    (backward-char (length head))
			    (if (looking-at (regexp-quote head))
				(progn
				  (if (not prehead) (skip-chars-backward " \t"))
				  (delete-region (point) opoint)))))
		      (if tail
			  (save-excursion
                            (if pretail (skip-chars-forward " \t"))
			    (setq opoint (point))
			    (if (not pretail) (skip-chars-forward " \t"))
			    (if (looking-at (regexp-quote tail))
				(progn
				  (forward-char (length tail))
				  (if (not posttail) (skip-chars-forward " \t"))
				  (delete-region opoint (point))))))))

		(save-excursion
		  (beginning-of-line)
		  (setq empty
			(or
			 (looking-at "[ \t]*$")
			 (looking-at (concat "[ \t]*" comment-start-skip
					     "[ \t]*$"))
			 (and (boundp 'comment-line-start-skip)
                              (looking-at (concat comment-line-start-skip
                                                  "[ \t]*$")))
			 (looking-at "     [^ 0\n][ \t]*$")))
		  (if empty
		      (progn
			(delete-region (match-beginning 0) (match-end 0))
			(if (looking-at "\n") (delete-char 1)))))
		(if empty (end-of-line 0))
;		(setq limit
;		      (+ (point)
;			 (* (if edstar-forward 1 -1)
;			    (window-width) (- (window-height) 1))))
		(setq limit
		      (+ (point)
			 (* (if t 1 -1)
			    (window-width) (- (window-height) 1))))
		(save-excursion
		  (edstar-best-place t limit)
		  (setq next (point)))
		(if (pos-visible-in-window-p next)
		    (goto-char next)
		  (message "No nearby placeholder found"))))))))

;;; Default definition of self-insert function that caters for placeholder
;;; expansion.
;;; NB a copy of this is held in loaddefs.el - update this if the function
;;; is changed!!
;;;
;;;###autoload
(edstar-virtual edstar-self-insert (&optional arg)
 (interactive)
 (let ((place nil))
    (if (and (not arg) edstar-auto-place-start (= last-command-char ? ))
	(edstar-expand-auto-place))

;;; Determine if we are positioned on a placeholder, without changing the
;;; current editing position.
    (save-excursion (setq place (edstar-in-place)))

;;; If so, and we are not at the very start of the placeholder, then expand it
;;; to yield an empty value (this duplicates the placeholder and removes the
;;; original placeholder text if necessary).
    (if (and place (> (point) (cdr (assq 'beginning place))))
        (edstar-expand-place t))

;;; Insert the character supplied, or the last one typed. (Normally, no
;;; character will be supplied as an argument, but this facility is available
;;; so that other code may simulate the typing of characters by the user.)
    (if arg (insert arg) (self-insert-command 1))))

;;; Bind all necessary keys to EDSTAR version of self-insert.
;;;###autoload
(substitute-key-definition 'self-insert-command 'edstar-self-insert global-map)

;;;###autoload
(defun edstar-expand-auto-place ()
  (if edstar-auto-place-start
      (let ((end (point)))
	(save-excursion
	  (if
	      (re-search-forward
	       (concat "\\("
		       (regexp-quote
			(concat
			 edstar-placeholder-begin
			 edstar-auto-place-name
			 edstar-placeholder-end))
		       "\\)\\|\\("
		       (regexp-quote
			(concat
			 edstar-placeholder-begin-opt
			 edstar-auto-place-name
			 edstar-placeholder-end-opt))
		       "\\)") nil t nil)
	      (progn
		(goto-char (match-beginning 0))
		(delete-region (match-beginning 0) (match-end 0))
		(let* ((text (buffer-substring edstar-auto-place-start end))
		       (len (length text))
		       (i 0))
		  (while (< i len)
		    (edstar-self-insert (elt text i))
		     (setq i (1+ i)))))))
	(setq edstar-auto-place-start nil))))

;;;###autoload
(defun edstar-next-line ()
  (interactive)
  (let ((goal-column 0)
	(next-line-add-newlines nil))
    (if edstar-forward
	(next-line 1)
      (if (bolp)
	  (next-line -1)
	(beginning-of-line)))))

;;;###autoload
(defun move-left ()
  (interactive)
  (backward-char 1))

;;;###autoload
(defun move-right ()
   (interactive)
   (forward-char 1))

;;;###autoload (defvar edstar-goal-column nil)

;;;###autoload
(defun move-up ()
  (interactive)
  (let ((tmp goal-column) goal-column)
    (if (not (or (equal last-command 'move-up)
		 (equal last-command 'move-down)))
	(setq edstar-goal-column (current-column)))
    (setq goal-column (or tmp edstar-goal-column))
    (previous-line 1)))

;;;###autoload
(defun move-down ()
  (interactive)
  (let ((tmp goal-column) goal-column
	(next-line-add-newlines nil))
    (if (not (or (equal last-command 'move-up)
		 (equal last-command 'move-down)))
	(setq edstar-goal-column (current-column)))
    (setq goal-column (or tmp edstar-goal-column))
    (next-line 1)))

;;;###autoload
(defun expand-indicated ()
  (interactive)
  (let ((opoint (point)))
    (edstar-expand)
    (save-excursion
      (goto-char opoint)
      (edstar-align-trailing)))
  (edstar-align-trailing))

;;;###autoload
(defun erase-placeholder ()
  (interactive)
  (let ((opoint (point)))
    (edstar-kill-place)
    (save-excursion
      (goto-char opoint)
      (edstar-align-trailing)))
  (edstar-align-trailing))

;;;###autoload
(defun next-placeholder ()
   (interactive)
   (edstar-align-trailing)
   (edstar-expand-auto-place)
   (edstar-goto-place t)
   (edstar-align-trailing))

;;;###autoload
(defun previous-placeholder ()
   (interactive)
   (edstar-align-trailing)
   (edstar-expand-auto-place)
   (edstar-goto-place nil)
   (edstar-align-trailing))
