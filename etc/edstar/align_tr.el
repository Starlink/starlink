;;;###autoload
(defun edstar-align-trailing ()
  (interactive)

;;; Take no action unless a trailing comment column and trailing comment
;;; regexp are defined.
  (if (and comment-column comment-start-skip)

;;; Define local variables.
      (let (comment quoted (last -1))

;;; Save the current buffer context and move to the start of the current line.
	(save-excursion
	  (beginning-of-line)

;;; Note if we are looking at the start of a full-line comment (or at a
;;; trailing comment which starts at the beginning of the line). If so, then
;;; no further action is needed.
	  (if (or (and (boundp 'comment-line-start-skip)
                       (looking-at comment-line-start-skip))
                  (looking-at comment-start-skip))
              ()

;;; Otherwise, loop to inspect each character between the beginning and end
;;; of the line, looking for an end-of-line comment character.
	    (while (and (not (eolp)) (not comment))

;;; Take account of quotes. Note if an unquoted start of a comment is found.
;;; Also note if any non-whitespace characters are found before the comment.
	      (if (looking-at "'") (setq quoted (not quoted)))
	      (if (and (not quoted)
		       (looking-at comment-start-skip)) (setq comment (point)))
	      (if (and (not comment)
		       (not (looking-at "[ \t]"))) (setq last (point)))

;;; Increment to inspect the next character.
	      (forward-char))

;;; If a trailing comment was found, save the buffer context and move to
;;; it.
	    (if comment
		(save-excursion
		  (goto-char comment)

;;; Delete preceding white space to move the comment left if necessary,
;;; always leaving one space after the preceding text.
		  (while (and (> (current-column) comment-column)
			      (> (point) (+ last 2)))
		    (backward-delete-char 1))

;;; Similarly, move the comment right if necessary.
		  (while (or (< (current-column) comment-column)
			     (< (point) (+ last 2)))
		    (insert-char ?  1)))))))))
