;;;###autoload (defvar edstar-token-table nil)
;;;###autoload (defvar edstar-token-table-size 4095)

;;; Create a new token (and a table to hold it if necessary).
;;;###autoload
(defun new-token (name value &optional prop mode)
"
Purpose:
   Define a new token.

Description:
   The function makes a new entry in the keyword table describing a token.
   If a token of the same name already exists, it is over-written.

Arguments:
   name
      A string containing the name of the token (case insensitive).
   value
      The value of the token. This may be a:

         string
            Interpreted as a keyboard macro used to generate the token
            expansion.
         (other types to be added...)
   prop (optional)
      If given, this should be a alist which defines a list of token
      properties in the form ((pname . pval) (pname . pval) ...), where:

         pname
            Identifies the property (normally a symbol).
         pval
            Is the property's value, whose type should be appropriate to the
            property being defined.
   mode
      The name of the major mode whose table is to receive the new token.
      If not given, the current token table is used if any. Otherwise, a
      token table is created for the current major mode and that is used
      instead.

Returned Value:
   nil
"

;;; Define local variables.
  (let (sym table)

;;; If the major mode was specified, obtain the symbol that identifies the
;;; token table associated with that mode. Create it if necessary.
    (if mode
	(setq table (intern (concat mode "-mode-token-table")))

;;; Otherwise, get the symbol for the current token table, if defined.
      (if edstar-token-table
	  (setq table edstar-token-table)

;;; If no current table is defined, then obtain the symbol that identifies the
;;; token table for the current major mode. Create it if necessary.
	(setq table (intern (concat (symbol-name major-mode)
				    "-token-table")))))

;;; If the symbol identifying the table is unbound (i.e. the table has not
;;; yet been created), then create an obarray in the symbols value cell to
;;; hold the table.
    (if (not (boundp table))
	(set table (make-vector edstar-token-table-size 0)))

;;; Intern a symbol in the table obarray to identify the new token and set
;;; its value to the placeholder translation value.
    (setq sym (intern (upcase name) (symbol-value table)))
    (set sym value)

;;; Set the property list (if supplied), prepending ((class . token)) to it
;;; to identify this entry as a token.
    (setplist sym
	      (append (list '(class . token)) prop)))

;;; Return nil.
  nil)
