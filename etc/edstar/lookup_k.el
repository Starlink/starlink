;;;###autoload
(defun edstar-lookup-keyword (name &optional table)
"
Purpose:
   Look up a keyword in a table.

Description:
   Given a string containing the name (or an abbreviation) for a keyword
   (e.g. a token or helpkey) which is to be expanded, this function validates
   the name by looking it up in a keyword table. If found, it returns a symbol
   which contains the keyword information.

Arguments:
   name
      A string containing the keyword name, or an abbreviation (case
      insensitive). Abbreviations need not be unique; if necessary, a menu
      will be presented to the user to resolve any ambiguity.
   table
      An optional keyword table in which to find the name. If not given,
      edstar-token-table is used by default.

Returned Value:
   If the keyword name is valid, this function returns the symbol (interned
   in the table) which contains the keyword expansion information. Otherwise
   it returns nil.

Notes:
   If user interaction is necessary, then the user will have the option of
   aborting the keyword look-up. If this occurs, a value of nil will be
   returned, even though the keyword abbreviation was valid.
"

;;; Define local variables.
  (let (list exact len next sym desc plist menu choice)

;;; Supply a default table.
    (if (not table) (setq table edstar-token-table))

;;; Obtain a list of all possible completions of the name given using the set
;;; of symbols in the token table. (Note the symbols in this table are all in
;;; upper case.) If there are no possible completions, return nil.
    (if (not (setq list
		   (all-completions (upcase name) (symbol-value table))))
	nil

;;; Search the list of completions to see if any is an exact match.
      (setq next list)
      (while (and next (not exact))
	(if (string= (car next) (upcase name)) (setq exact t))
	(setq next (cdr next)))

;;; Obtain the number of possible completions. If there is only one, or there
;;; was an exact match, look it up in the token table and return the
;;; corresponding symbol.
      (setq len (length list))
      (if (= len 1)
	  (intern-soft (elt list 0) (symbol-value table))
	(if exact
	    (intern-soft (upcase name) (symbol-value table))

;;; If there is more than one completion, then user input must be obtained
;;; to resolve the ambiguity. Sort the completions into reverse alphabetical
;;; order.
	  (setq list (sort list '(lambda (str1 str2) (string< str2 str1))))

;;; Loop to construct a list containing menu items as strings.
	  (setq next list)
	  (while next

;;; Look up the symbol corresponding to each possible completion in the
;;; token table.
	    (setq sym (intern-soft (car next) (symbol-value table)))

;;; Obtain a one-line description for the keyword.
	    (setq desc (edstar-get-desc-string (symbol-name sym) 'token))

;;; Cons a string for each menu item on to the front of the menu list being
;;; built.
	    (setq menu (cons (concat (car next) " : " desc) menu))

;;; Increment to look at the next possible completion.
	    (setq next (cdr next)))

;;; Allow the user to choose from the resulting menu.
	  (if (setq choice (edstar-choose-from-menu menu))

;;; If a valid choice was made, find the chosen keyword completion string and
;;; look it up in the token table. Return the resulting symbol.
	      (intern-soft (elt list (- (- len choice) 1))
                           (symbol-value table))

;;; If the menu was aborted, return nil.
	    nil))))))
