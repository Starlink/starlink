(if window-system
    (progn
      (defvar edstar-last-hi-place-ov nil)
      (make-variable-buffer-local 'edstar-last-hi-place-ov)

      (defvar edstar-hi-place-background "firebrick3")
      (copy-face 'default 'edstar-hi-place-face)
      (set-face-background 'edstar-hi-place-face edstar-hi-place-background)

      (defun edstar-hi-place ()
	(interactive)
	(if edstar-last-hi-place-ov
	    (progn
	      (delete-overlay edstar-last-hi-place-ov)
	      (setq edstar-last-hi-place-ov nil)))
	(save-excursion
	  (let (place start end ov)
	    (setq place (edstar-in-place))
	    (if place
		(progn
		  (setq start (cdr (assq 'beginning place)))
		  (setq end (cdr (assq 'end place)))

		  (setq ov (make-overlay start end))
		  (overlay-put ov 'priority 100)
		  (overlay-put ov 'face 'edstar-hi-place-face)
		  (setq edstar-last-hi-place-ov ov))))))

      (add-hook 'post-command-hook 'edstar-hi-place t)))
