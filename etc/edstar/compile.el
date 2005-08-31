;;;###autoload (defvar edstar-error-index 0)

;;;###autoload
(defun com ()
   (interactive)
   (let ((compilation-ask-about-save nil)
         )
     (compile compile-command)))

;;;###autoload
(defun forward-error ()
  (interactive)
  (next-error)
  (setq edstar-error-index (1+ edstar-error-index)))

;;;###autoload
(defun backward-error ()
  (interactive)
  (if (<= edstar-error-index 1)
      (error "At start of list")
    (let ((i 1))
      (next-error t)
      (while (< i (- edstar-error-index 1))
	(next-error)
	(setq i (1+ i)))
      (setq edstar-error-index (max 0 (- edstar-error-index 1))))))
