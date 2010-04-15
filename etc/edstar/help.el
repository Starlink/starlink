;;;###autoload (defvar edstar-auto-help-flag t)

;;;###autoload
(defun auto-help ()
  (interactive)
  (setq edstar-auto-help-flag
        (if edstar-auto-help-flag nil t)))

;;;###autoload
(defun edstar-help-on-this (&optional name)
  (interactive)
  (let (keyword value)
    (setq keyword (edstar-find-helpkey name))
    (if keyword
        (progn
          (setq name (cdr (assq 'name keyword)))
          (setq value (cdr (assq 'value keyword)))
          (message (concat "Requesting external help on \"" name "\"."))

          (funcall
           (list 'lambda () (append '(call-process "showme" nil 0 nil) value)))

;          (if (/= status 0)
;              (message (concat "No external help available on \"" name "\"."))
;            (message (concat "External help on \"" name "\" found OK.")))
          ))))

;;; Look up a helpkey in the helpkey table and return an alist containing
;;; information about it. If no helpkey name is given, the name is obtained
;;; from the current buffer at the editing position. In this case (only),
;;; the returned alist will also contain the "beginning" and "end" locations
;;; of the text used.
;;;###autoload
(defun edstar-find-helpkey (&optional name)
  (interactive)
  (let (helpkey found)

;;; If a helpkey name was given, initialise an alist to contain the name
;;; string.
    (if name
        (setq helpkey (list (cons 'name name)))

;;; Otherwise, search for a suitable name in the current editing buffer.
      (if (setq helpkey (edstar-find-keyword-string))

;;; If successful, extract the helpkey name from the returned alist.
          (setq name (cdr (assq 'name helpkey)))

;;; Otherwise, issue an error message.
        (error "Nothing here to obtain help on.")))

;;; Look up the helpkey name in the helpkey table. If unsuccessful, report
;;; an error.
    (if (not (setq found (edstar-lookup-keyword name edstar-helpkey-table)))
        (error (concat "No help available on \"" name "\"."))

;;; Add the helpkey translation value and property list to the returned alist.
      (append
       (list (cons 'value (symbol-value found))
             (cons 'plist (symbol-plist found)))
       helpkey))))
