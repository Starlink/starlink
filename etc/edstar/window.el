;;; Return line number of point in current window.
;;;###autoload
(defun edstar-current-line-in-window ()
  (+ (count-lines (window-start) (point))
     (if (= (current-column) 0) 0 -1)))

