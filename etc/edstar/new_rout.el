;;;###autoload
(defun new-routine (name desc args &optional prop mode)
  (let (val)
    (setq val name)
    (if args
	(let ((first t))
	  (setq val (concat val "("))
	  (while args
	    (new-place (car args)
		       nil
		       '((help . "Please supply an argument value."))
                       mode)
	    (setq val (concat val
			      (if first
                                  (concat " " edstar-placeholder-begin)
                                (concat ", " edstar-placeholder-begin))
			      (downcase (car args))
                              edstar-placeholder-end))
	    (setq first nil)
	    (setq args (cdr args)))
	  (setq val (concat val " )"))))
;    (new-token name val (cons (cons 'desc desc) nil))))
    (new-token name val (append (list (cons 'desc desc))
                                prop))))
