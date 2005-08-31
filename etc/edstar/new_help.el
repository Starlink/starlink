;;;###autoload (defvar edstar-helpkey-table nil)
;;;###autoload (defvar edstar-helpkey-table-size 4095)

;;; Create a new helpkey (and a table to hold it if necessary).
;;;###autoload
(defun new-helpkey (name value &optional prop mode)
"
Purpose:
   Define a new helpkey.

Description:
   The function makes a new entry in the keyword table describing a helpkey.
   If a helpkey of the same name already exists, it is over-written.

Arguments:
   name
      A string containing the name of the helpkey (case insensitive). This
      is a string of text which may appear in an editing buffer and on which
      help information may be requested.
   value
      A list containing a sequence of help keys, as strings, which will be
      used to locate the help information.
   prop (optional)
      If given, this should be a alist which defines a list of helpkey
      properties in the form ((pname . pval) (pname . pval) ...), where:

         pname
            Identifies the property (normally a symbol).
         pval
            Is the property's value, whose type should be appropriate to the
            property being defined.
   mode
      The name of the major mode whose table is to receive the new helpkey.
      If not given, the current helpkey table is used if any. Otherwise, a
      helpkey table is created for the current major mode and that is used
      instead.

Returned Value:
   nil
"

;;; Define local variables.
  (let (sym table)

;;; If the major mode was specified, obtain the symbol that identifies the
;;; helpkey table associated with that mode. Create it if necessary.
    (if mode
	(setq table (intern (concat mode "-mode-helpkey-table")))

;;; Otherwise, get the symbol for the current helpkey table, if defined.
      (if edstar-helpkey-table
	  (setq table edstar-helpkey-table)

;;; If no current table is defined, then obtain the symbol that identifies the
;;; helpkey table for the current major mode. Create it if necessary.
	(setq table (intern (concat (symbol-name major-mode)
				    "-helpkey-table")))))

;;; If the symbol identifying the table is unbound (i.e. the table has not
;;; yet been created), then create an obarray in the symbol's value cell to
;;; hold the table.
    (if (not (boundp table))
	(set table (make-vector edstar-helpkey-table-size 0)))

;;; Intern a symbol in the table obarray to identify the new helpkey and set
;;; its value to the placeholder translation value.
    (setq sym (intern (upcase name) (symbol-value table)))
    (set sym value)

;;; If a helpkey property list was also given, add this to the symbol.
    (if prop (setplist sym prop)))

;;; Return nil.
  nil)
