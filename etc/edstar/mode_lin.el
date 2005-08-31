;;; Define a variable to hold the current column in each buffer.
(defvar edstar-current-column 0)
(make-variable-buffer-local 'edstar-current-column)

;;; Define a function to record the current column after each command.
(defun edstar-save-current-column ()
  (setq edstar-current-column (format "%d" (1+ (current-column)))))

;;; Arrange for this function to be run after each command.
(add-hook 'post-command-hook 'edstar-save-current-column t)

;;; Set the mode line format to include the column and line numbers.
(setq-default mode-line-format
  (list (purecopy "")
   'mode-line-modified
   'mode-line-buffer-identification
   (purecopy "   ")
   'global-mode-string
   (purecopy "   %[(")
   'mode-name 'mode-line-process 'minor-mode-alist
   (purecopy "%n")
   (purecopy ")%]--")
   (purecopy "(" )
   (purecopy 'edstar-current-column)
   (purecopy ",%l)--" )
   (purecopy '(-3 . "%p"))
   (purecopy "-%-")))
