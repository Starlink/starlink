;;; Dump a list of obarrays to a lisp file (which can later re-create them).

;;;###autoload
(defun edstar-dump-tables (tablelist file &optional feature)
  (let (out table)
    (save-excursion
      (setq out (find-file file))
      (erase-buffer)
      (if feature
	  (progn
	    (insert "(provide '")
	    (prin1 feature out)
	    (insert ")\n")))
      (while tablelist
	(setq table (car tablelist))
        (insert "(defvar ")
        (prin1 table out)
        (insert " (make-vector ")
        (prin1 (length (symbol-value table)) out)
        (insert " 0))\n")

	(insert "(let (sym)")
	(mapatoms 'edstar-dump-symbol (symbol-value table))
	(insert ")\n")
	(setq tablelist (cdr tablelist)))
      (save-buffer))
    (kill-buffer out)))
;    (byte-compile-file file)))

;;;###autoload
(defun edstar-dump-symbol (sym)
  (let (val prop)
    (insert "\n  (setq sym (intern \"")
    (prin1 sym out)
    (insert "\" ")
    (prin1 table out)
    (insert "))\n")

    (setq val (symbol-value sym))
    (insert "  (set sym ")
    (if (not (stringp val)) (insert "'"))
    (prin1 val out)
    (insert ")\n")

    (setq prop (symbol-plist sym))
    (if prop
	(progn
	  (insert "  (setplist sym '")
	  (prin1 prop out)
	  (insert ")\n")))))
