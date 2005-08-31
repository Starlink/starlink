;;; Get a description string for a token or placeholder.
;;;###autoload
(defun edstar-get-desc-string (name type)
  (interactive)

;;; Local variables.
  (let (sym plist desc value)

;;; Look up the symbol describing the token or placeholder in the appropriate
;;; table.
    (if (eq type 'token)
	(setq sym (intern-soft (upcase name)
			       (symbol-value edstar-token-table))))
    (if (eq type 'place)
	(setq sym (intern-soft (upcase name)
			       (symbol-value edstar-placeholder-table))))

;;; If found, obtain its property list.
    (if sym
	(progn
	  (setq plist (symbol-plist sym))

;;; If the property list exists, extract the value of the "desc" property,
;;; which contains any description string.
	  (if plist
	      (setq desc (cdr (assq 'desc plist))))

;;; If this did not yield a value, get the expansion value.
	  (if (not desc)
	      (progn
		(setq value (symbol-value sym))

;;; If the expansion value is a string, then use that instead.
		(cond
		 ((stringp value)
		  (setq desc value))

;;; If the expansion value is a list (i.e. a menu) with only one element, then
;;; get the name value and expansion type for the sole element.
		 ((and (listp value) (= (length value) 1))
		  (let (name1 value1 type1)
		    (setq name1 (elt (car value) 0))
		    (setq value1 (elt (car value) 1))
		    (setq type1 (elt (car value) 2))
		    (if (stringp value1)
			(setq desc value1)
		      (setq desc (edstar-get-desc-string name1 type1))))))))))

;;; Extract just the first line of a multi-line string.
    (if desc (setq desc (edstar-first-line-of-string desc)))

;;; Return the description string.
    desc))
