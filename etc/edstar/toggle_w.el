;;;###autoload
(defun edstar-toggle-windows ()
"
Purpose:
   Toggle the number of visible screen windows between 1 and 2.

Description:
   If there is only one window, then it is split horizontally into two equal
   windows. If more than one is visible, then all except the current window
   is deleted, leaving only a single window.
"
   (interactive)

;;; Decide how many windows there are and act accordingly.
   (if (one-window-p)

;;; If necessary, split the window into two.
       (split-window)

;;; Otherwise delete other windows in the same frame.
     (delete-other-windows)))
