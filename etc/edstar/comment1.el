;;;###autoload
(defun edstar-insert-comment-line ()
"
Purpose:
   Insert a comment about the current line.

Description:
   The function inserts a new full comment line (containing a comment
   placeholder) in front of the current line and moves the editing position
   (point) on to the placeholder, ready to enter a new comment.

Notes:
   -  This command takes no action if the current line is already a full
      comment line.
   -  If the previous line is not already blank, a blank line will be inserted
      in front of the new comment line, to separate it from previous text.
"
  (interactive)

;;; Local variables.
  (let (bol comment blank (start nil))

;;; Save the buffer position and move to the start of the current line.
    (save-excursion
      (beginning-of-line)

;;; Remember this position for later and test whether the line is a comment
;;; line (starts with a line comment character sequence or a trailing comment
;;; character sequence).
      (setq bol (point))
      (setq comment (or (and (and (boundp 'comment-line-start-skip)
                                  comment-line-start-skip)
                             (looking-at comment-line-start-skip))
                        (and (and (boundp 'comment-start-skip)
                                  comment-start-skip)
                             (looking-at comment-start-skip))))

;;; If the line is already a comment, there is nothing more to do. Otherwise,
;;; return to the beginning of the line and test if the previous line is
;;; blank.
    (if (not comment)
	(progn
	  (goto-char bol)

;;; For this purpose, the beginning of the buffer counts as a blank line. If
;;; we are not at the beginning of the buffer, inspect the previous line
;;; explicitly to see if it is completely blank.
	  (if (not (setq blank (bobp)))
	      (save-excursion
		(beginning-of-line 0)
		(setq blank (looking-at "^[ \t]*$"))))

;;; If the previous line is not blank, insert a blank line as spacing before
;;; the comment.
	  (if (not blank) (insert "\n"))

;;; If available, insert a full-line comment.
	  (save-excursion
            (if (and (boundp 'comment-line-start) comment-line-start)
                (progn
                  (insert comment-line-start)
                  (insert edstar-placeholder-begin-opt)
                  (setq start (point))  ; Remember start of placeholder
                  (insert "comment")
                  (insert edstar-placeholder-end-opt)
                  (insert "\n"))

;;; Otherwise, if available, insert a trailing comment (starting at the
;;; beginning of the line).
              (if (and (boundp 'comment-start) comment-start)
                  (progn
                    (insert comment-start)
                    (insert edstar-placeholder-begin-opt)
                    (setq start (point))  ; Remember start of placeholder
                    (insert "comment")
                    (insert edstar-placeholder-end-opt)

;;; Append an end-of-comment sequence if needed.
                    (if (and (boundp 'comment-end) comment-end)
                        (insert comment-end))
                    (insert "\n"))))))))

;;; Move point on to the placeholder.
    (if start
        (progn
          (goto-char start)
          (indent-according-to-mode)))))
