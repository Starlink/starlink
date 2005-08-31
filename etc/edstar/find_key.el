;;;###autoload
(defun edstar-find-keyword-string ()
"
Purpose:
   Find the text of a keyword to be expanded.

Description:
   The function searches for a word (or partial word) in the current buffer
   immediately preceding the editing position (point). If such a word is
   found, a description of it is returned. Otherwise nil is returned.

Returned Value:
   If a keyword is found, the returned value is a data structure consisting of
   an alist containing the following symbols and their associations:

      beginning
         The position of the beginning of the keyword in the buffer.
      end
         The position of the end of the keyword in the buffer.
      name
         A string containing a copy of the keyword text.
"

;;; Define local variables.
  (let (found)

;;; Save the current buffer context and search backwards for the start of
;;; a word followed by at least one word-constituent character.
    (save-excursion
      (re-search-forward "\\>" nil t nil)
      (setq found (re-search-backward "\\<\\sw+" nil t nil)))

;;; If found, and the matched pattern ends at point (i.e. the word is
;;; immediately in front of the editing position), then return an alist
;;; containing a description for the identified keyword.
    (if (and found (and (<= (match-beginning 0) (point))
                        (>= (match-end 0) (point))))
	(list (cons 'beginning (match-beginning 0))
	      (cons 'end (match-end 0))
	      (cons 'name (buffer-substring (match-beginning 0)
                                            (match-end 0))))

;;; If not found, return nil.
      nil)))
