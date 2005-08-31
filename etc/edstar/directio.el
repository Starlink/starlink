;;;###autoload (defvar edstar-forward t)
;;;###autoload (make-variable-buffer-local 'edstar-forward)

;;;###autoload
(defun edstar-set-forward ()
   (interactive)
   (setq edstar-forward t))

;;;###autoload
(defun edstar-set-reverse ()
   (interactive)
   (setq edstar-forward nil))
