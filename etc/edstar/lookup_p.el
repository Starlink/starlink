;;; Loop up a placeholder in the current placeholder table.
;;;###autoload
(defun edstar-lookup-place (name)
  (intern-soft (upcase name) (symbol-value edstar-placeholder-table)))


