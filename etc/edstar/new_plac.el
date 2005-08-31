;;;###autoload (defvar edstar-placeholder-table nil)
;;;###autoload (defvar edstar-placeholder-table-size 4095)

;;; Create a new placeholder (and a table to hold it if necessary).
;;;###autoload
(defun new-place (name value &optional prop mode)
  (let (sym table)

;;; If the major mode was specified, obtain the symbol that identifies the
;;; placeholder table associated with that mode. Create it if necessary.
    (if mode
        (setq table (intern (concat mode "-mode-placeholder-table")))

;;; Otherwise, get the symbol for the current placeholder table, if defined.
      (if edstar-placeholder-table
	  (setq table edstar-placeholder-table)

;;; If no current table is defined, then obtain the symbol that identifies the
;;; placeholder table for the current major mode. Create it if necessary.
	(setq table (intern (concat (symbol-name major-mode)
				    "-placeholder-table")))))

;;; If the symbol identifying the table is unbound (i.e. the table has not
;;; yet been created), then create an obarray in the symbols value cell to
;;; hold the table.
    (if (not (boundp table))
	(set table (make-vector edstar-placeholder-table-size 0)))

;;; Intern a symbol in the table obarray to identify the new placeholder and
;;; set its value to the placeholder translation value.
    (setq sym (intern (upcase name) (symbol-value table)))
    (set sym value)

;;; If a placeholder property list was also given, add this to the symbol.
    (if prop (setplist sym prop)))

;;; Return nil.
  nil)
