;;;###autoload
(defun edstar-enable-screen-limiting (ncols) )

(if (not window-system)
    ()
  (defvar edstar-first-buffer-change nil)
  (make-variable-buffer-local 'edstar-first-buffer-change)

  (defvar edstar-last-buffer-change nil)
  (make-variable-buffer-local 'edstar-last-buffer-change)

  (defvar edstar-right-screen-limit nil)
  (make-variable-buffer-local 'edstar-right-screen-limit)

;;; Make a bold face to mark over-long lines.
  (defvar edstar-screen-limit-face
    (progn
      (copy-face 'default 'edstar-screen-limit-face)
      (set-face-background 'edstar-screen-limit-face "red")))

;;; Set overlays to mark lines which are too long.
  (defun edstar-set-screen-limits (start end)
    (if edstar-right-screen-limit
	(let (pos last more ov eol)
	  (setq pos (save-excursion
		      (goto-char start)
		      (beginning-of-line)
		      (point)))
	  (setq last (save-excursion
		       (goto-char end)
		       (end-of-line)
		       (point)))
	  (save-excursion
	    (goto-char pos)
	    (setq more t)
	    (while more
	      (save-excursion
		(end-of-line)
		(if (> (current-column) edstar-right-screen-limit)
		    (progn
		      (setq eol (point))
		      (move-to-column edstar-right-screen-limit)
		      (setq ov (make-overlay (point) eol))
		      (overlay-put ov 'priority 10000)
		      (overlay-put ov 'face 'edstar-screen-limit-face)
		      (overlay-put ov 'edstar-screen-limit t))))
	      (setq more (and (= (forward-line 1) 0) (<= (point) last))))))))

  (defun edstar-zap-screen-limits (start end)
    (let (pos last more ov ovlist)
      (setq pos (save-excursion
		  (goto-char start)
		  (beginning-of-line)
		  (point)))
      (setq last (save-excursion
		   (goto-char end)
		   (end-of-line)
		   (point)))
      (setq more t)
      (while more
	(setq pos (next-overlay-change pos))
	(if (< pos last)
	    (progn
	      (setq ovlist (overlays-at pos))
	      (while ovlist
		(setq ov (car ovlist))
		(if (overlay-get ov 'edstar-screen-limit) (delete-overlay ov))
		(setq ovlist (cdr ovlist))))
	  (setq more nil)))))

;;; Accumulate start and end of successive changes to the current buffer
;;; (accumulated changes are buffer-local variables).
  (defun edstar-accumulate-buffer-changes (start end length)
    (if edstar-first-buffer-change
	(progn
	  (setq edstar-first-buffer-change
		(min edstar-first-buffer-change start))
	  (setq edstar-last-buffer-change
		(max edstar-last-buffer-change end)))
      (setq edstar-first-buffer-change start)
      (setq edstar-last-buffer-change end)))

  (defun edstar-update-screen-limits ()
    (if (and edstar-right-screen-limit edstar-first-buffer-change)
	(progn
	  (edstar-zap-screen-limits edstar-first-buffer-change
				    edstar-last-buffer-change)
	  (edstar-set-screen-limits edstar-first-buffer-change
				    edstar-last-buffer-change)
	  (setq edstar-first-buffer-change nil)
	  (setq edstar-last-buffer-change nil))))

  (defun edstar-enable-screen-limiting (ncols)
    (edstar-zap-screen-limits (point-min) (point-max))
    (setq edstar-right-screen-limit ncols)
    (edstar-set-screen-limits (point-min) (point-max))
    (make-local-variable 'after-change-function)
    (setq after-change-function 'edstar-accumulate-buffer-changes)
    (make-local-variable 'post-command-hook)
    (add-hook 'post-command-hook 'edstar-update-screen-limits t)))
