(defvar edstar-top-scroll-border 20)
(defvar edstar-bottom-scroll-border 20)

;;; Adjust screen scrolling to implement "scrolling borders" and to prevent
;;; scrolling beyond the ends of the file.
(defun edstar-check-scroll ()

;;; Define local variables.
  (let (done start window top bot current last nlines move)

;;; Loop through all displayed windows.
    (setq start (selected-window))
    (setq window start)
    (while (not done)

;;; Determine the number of window lines available for displaying text and
;;; the line number of the last line.
      (setq nlines (1- (window-height)))
      (setq last (1- nlines))

;;; Determine the width in lines of the top scrolling border.
      (setq top (/ (* nlines edstar-top-scroll-border) 100))

;;; Determine which line of the current window point is in.
      (setq current (+ (count-lines (window-start) (point))
		       (if (= (current-column) 0) 0 -1)))

;;; If the current line lies above the border, then scroll to bring the
;;; line containing point to the border position.
      (if (< current top)
	  (progn
	    (recenter top)
	    (setq current top))

;;; If no scrolling was needed to satisfy the top border, then examine
;;; the bottom border. Find the window line corresponding to this border
;;; width.
	(setq bot (- last (/ (* nlines edstar-bottom-scroll-border) 100)))

;;; If point lies below this line, then scroll to bring the line containing
;;; point up to the bottom border position.
	(if (> current bot)
	    (progn
	      (recenter bot)
	      (setq current bot))))

;;; This may have exposed the end of the file. To test for this, we save
;;; the current position and move down sufficient lines to reach the bottom of
;;; the window. Calculate how many lines movement are required.
      (save-excursion
	(setq move (- last current))

;;; If the actual motion which results is less than requested, then we have
;;; hit the end of the file. In this case, re-scroll to put the last line of
;;; the file at the bottom of the screen.
	(if (< (vertical-motion move) move) (recenter last)))

;;; Select the next displayed window and test if we have returned to the
;;; original (starting) window.
      (setq window (next-window window nil t))
      (select-window window)
      (setq done (equal window start)))))

;;; Establish the scroll checking hook.
      (add-hook 'post-command-hook 'edstar-check-scroll t)
