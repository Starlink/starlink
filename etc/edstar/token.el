;;;###autoload (defvar edstar-indent 3)
;;;###autoload (defvar edstar-auto-place-start nil)
;;;###autoload (defvar edstar-auto-place-name nil)

;;;###autoload
(defun edstar-find-keyword ()
  (interactive)
  (let (keyword name found)
    (if (not (setq keyword (edstar-find-keyword-string)))
	(progn
	  (message "Nothing to expand here.")
	  nil)
      (setq name (cdr (assq 'name keyword)))
      (if (not (setq found (edstar-lookup-keyword name)))
	  (progn
	    (message "Nothing to expand here.")
	    nil)
	(append
	 (list (cons 'value (symbol-value found))
	       (cons 'plist (symbol-plist found)))
	 keyword)))))

;;;###autoload
(defun edstar-insert-expansion-text (text)
  (let (len i c)
    (setq len (length text))
    (setq i 0)
    (while (< i len)
      (setq c (aref text i))
      (cond
       ((= c ?\t)
	(indent-according-to-mode))
       ((= c ?\n)
	(edstar-breakline)       ; Break the current line at point,
                                 ; continuing any string quotes, etc.
        (indent-according-to-mode)) ; Indent the new line appropriately
       ((= c ?\\)
	(let ((start (point)))
	  (beginning-of-line)
	  (delete-region start (point))))
       ((= c ?\f)
	(insert-char ? edstar-indent))
       ((= c ?\b)
	(backward-delete-char-untabify edstar-indent))
       (t
	(insert-char c 1)))
      (setq i (1+ i)))))

;;;###autoload
(defun edstar-expand ()
  (interactive)
  (if (edstar-in-place)
      (progn
	(message "expanding placeholder")
	(edstar-expand-place))
    (message "expanding keyword")
    (edstar-expand-keyword)))

;;;###autoload
(defun edstar-expand-keyword ()
  (interactive)
  (let (keyword plist helpkey)
    (if (setq keyword (edstar-find-keyword))
        (progn
          (edstar-expand-token keyword)
          (if edstar-auto-help-flag
              (progn
                (setq plist (cdr (assq 'plist keyword)))
                (if plist
                    (progn
                      (setq helpkey (cdr (assq 'helpkey plist)))
                      (if helpkey (edstar-help-on-this helpkey))))))))))

;;;###autoload
(defun edstar-expand-token (token)
  (interactive)
  (let (beginning end value menu text expand plist alias newpoint)
    (setq beginning (cdr (assq 'beginning token)))
    (setq end (cdr (assq 'end token)))
    (setq value (cdr (assq 'value token)))
    (setq plist (cdr (assq 'plist token)))
    (if plist (setq alias (cdr (assq 'alias plist))))
    (cond

;;; Value is a string.
     ((stringp value)
      (delete-region beginning end)
      (save-excursion
	(edstar-insert-expansion-text value)
	(setq newpoint (point)))
      (if alias
	  (goto-char newpoint)
	(edstar-best-place t)))

; Value is a lambda expression...
     ((and (listp value) (equal (car value) 'lambda))
      (delete-region beginning end)
      (save-excursion
	(setq value (funcall value))
	(if (stringp value) (edstar-insert-expansion-text value))
	(setq newpoint (point)))
      (if alias
	  (goto-char newpoint)
	(edstar-best-place t)))

;;; Value is a list (i.e. a menu).
     ((listp value)
      (if (= (length value) 1)
	  (setq choice 0)
	(setq next value)
	(while next

;;; Get the name, description and expansion type for each menu item.
	  (let (name desc type)
	    (setq name (elt (car next) 0))
	    (setq desc (elt (car next) 1))
	    (setq type (elt (car next) 2))

;;; If no description was given, search for a description string associated
;;; with whatever the item will be expanded as.
	    (if (not desc)
		(setq desc (edstar-get-desc-string name type))
	      (setq desc (edstar-first-line-of-string desc)))
	    (setq menu (cons (concat (upcase name) " : " desc) menu)))
	  (setq next (cdr next)))
	(setq menu (reverse menu))
	(setq choice (edstar-choose-from-menu menu)))
      (if choice
	  (progn
	    (setq text (elt (elt value choice) 0))
	    (setq expand (elt (elt value choice) 2))
	    (delete-region beginning end)
	    (cond
	     ((eq expand 'token)
	      (insert text)
	      (edstar-expand-keyword))
	     ((eq expand 'place)
	      (save-excursion (insert edstar-placeholder-begin text edstar-placeholder-end))
	      (edstar-best-place t)
	      (edstar-expand-place))
             ((stringp expand)
              (save-excursion
                (edstar-insert-expansion-text expand))
              (edstar-best-place t))
	     (t
	      (save-excursion
		(edstar-insert-expansion-text text))
	      (edstar-best-place t))))))
     (t
      (message "Unknown token expansion type")))))

;;; Expands nearest placeholder. Still experimental.
;;;###autoload
(defun edstar-expand-place (&optional novalue)
   (interactive)
   (let (dupe beginning end value plist menu auto vert sep)
     (if (not (setq place (edstar-best-place edstar-forward)))
	 (message "No more placeholders")
       (progn
	 (setq beginning (cdr (assq 'beginning place)))
	 (setq end (cdr (assq 'end place)))
	 (setq value (cdr (assq 'value place)))
	 (setq plist (cdr (assq 'plist place)))
	 (setq auto (cdr (assq 'auto plist)))
	 (setq dupe (cdr (assq 'dupe place)))
	 (setq name (cdr (assq 'name place)))
	 (setq sep (cdr (assq 'sep plist)))
	 (setq vert (cdr (assq 'vert plist)))
         (setq consume (cdr (assq 'consume plist)))

	 (setq edstar-auto-place-start (and auto beginning))
	 (setq edstar-auto-place-name (and auto name))

	 (cond

; Placeholder is being typed over...
	  (novalue
	   (goto-char beginning)
	   (delete-region beginning end)
	   (edstar-insert-duplicate-place place))

; Value is a string...
	  ((stringp value)
	   (goto-char beginning)
	   (delete-region beginning end)
	   (edstar-insert-duplicate-place place)
	   (save-excursion
	     (edstar-insert-expansion-text value)
	     (edstar-expand-auto-place))
	   (edstar-best-place t))

; Value is a lambda expression (function)...
	  ((and (listp value) (equal (car value) 'lambda))
	   (goto-char beginning)
	   (delete-region beginning end)
	   (edstar-insert-duplicate-place place)
	   (save-excursion
	     (setq value (funcall value))
	     (if (not value)
		 (edstar-show-place-help place)
	       (if (stringp value)
		   (edstar-insert-expansion-text value)))
	     (edstar-expand-auto-place))
	   (edstar-best-place t))

;;; Value is nil (no expansion provided).
	  ((not value)
	   (edstar-show-place-help place))

;;; Value is a list (i.e. a menu).
	  ((listp value)
	   (if (= (length value) 1)
	       (setq choice 0)
	     (setq next value)
	     (while next

;;; Get the name, description and expansion type for each menu item.
	       (let (name desc type)
		 (setq name (elt (car next) 0))
		 (setq desc (elt (car next) 1))
		 (setq type (elt (car next) 2))

;;; If no description was given, search for a description string associated
;;; with whatever the item will be expanded as.
		 (if (not desc)
		     (setq desc (edstar-get-desc-string name type))
		   (setq desc (edstar-first-line-of-string desc)))
		 (setq menu (cons (concat (upcase name) " : " desc)
				  menu)))
	       (setq next (cdr next)))
	     (setq menu (reverse menu))
	     (setq choice (edstar-choose-from-menu menu)))
	   (if choice
	       (progn
		 (setq text (elt (elt value choice) 0))
		 (setq expand (elt (elt value choice) 2))
		 (goto-char beginning)
		 (delete-region beginning end)
		 (edstar-insert-duplicate-place place)
		 (cond
		  ((eq expand 'token)
		   (insert text)
		   (edstar-expand-keyword))
		  ((eq expand 'place)
		   (save-excursion (insert edstar-placeholder-begin text edstar-placeholder-end))
		   (edstar-best-place t)
		   (edstar-expand-place))
                  ((stringp expand)
                   (save-excursion
                     (edstar-insert-expansion-text expand)
                     (edstar-expand-auto-place))
                   (edstar-best-place t))
		  (t
		   (save-excursion
		     (edstar-insert-expansion-text text)
		     (edstar-expand-auto-place))
		   (edstar-best-place t))))))
	  (t
	   (message "unknown placeholder expansion type")))))))

;;;###autoload
(defun edstar-show-place-help (place)
  (let (name opt plist help pname)
    (setq name (cdr (assq 'name place)))
    (setq opt (cdr (assq 'opt place)))
    (setq plist (cdr (assq 'plist place)))
    (setq help (cdr (assq 'help plist)))
    (if (not help)
	(setq help "Sorry, no help is available for this placeholder."))

;;; Save the state of the current buffer and window, so we can resume editing
;;; after displaying the help text.
    (save-excursion
      (save-window-excursion

;;; Make local copies of the placeholder delimiters we will require, since they
;;; are buffer-local and we are about to change buffers.
        (let ((placebegin (if opt edstar-placeholder-begin-opt
                            edstar-placeholder-begin))
               (placeend (if opt edstar-placeholder-end-opt
                           edstar-placeholder-end)))

;;; Create a buffer for the placeholder help text (if necessary) and make it
;;; current.
          (set-buffer (get-buffer-create "*Placeholder Help*"))

;;; Erase the help buffer's contents.
          (setq buffer-read-only nil)
          (erase-buffer)

;;; Construct the full placeholder name, with delimiters and display it in a
;;; suitable message on the mode line.
          (setq pname (concat placebegin name placeend))
          (setq mode-line-format
                (concat "  Help on placeholder " pname
                        " -- Press RET to continue..."))

;;; Insert the help text and return to the start of the buffer,
          (insert help)
          (goto-char (point-min))

;;; Set up the keymap, which simply performs an exit when return is pressed.
          (use-local-map (make-sparse-keymap))
          (local-set-key "\C-m" 'exit-recursive-edit)

;;; Make the buffer read-only and display it in a new window.
          (setq buffer-read-only t)
          (pop-to-buffer (current-buffer))

;;; Do a recursive edit in the new window. When this edit exits, the help
;;; window will be removed and the previous edit will be resumed.
          (recursive-edit))))))

;;;###autoload
(defun edstar-insert-duplicate-place (place)
  (let (name dupe plist sep vert)
    (setq name (cdr (assq 'name place)))
    (setq dupe (cdr (assq 'dupe place)))
    (setq plist (cdr (assq 'plist place)))
    (setq sep (cdr (assq 'sep plist)))
    (setq vert (cdr (assq 'vert plist)))
    (save-excursion
      (if dupe
	  (let (protect)
	    (save-excursion
              (insert "x")
              (if (setq protect (looking-at "[ \t]+[^ \t\n]"))
                  (progn
                    (insert "x") ; Split line between "xx" to prevent loss of
                                 ; leading/trailing white space
                    (backward-char)))
	      (if sep (insert sep))
	      (if vert (newline-and-indent))
	      (insert edstar-placeholder-begin-opt name edstar-placeholder-end-opt edstar-placeholder-dupe)
              (if protect (delete-char 1)))
	    (delete-char 1))))))

;;; Returns the first line of a string (i.e. the substring extending up to
;;; the first newline).
;;;###autoload
(defun edstar-first-line-of-string (str)
  (let (len i found)
    (setq len (length str))
    (setq i 0)
    (while (and (< i len) (not found))
      (if (= (elt str i) ?\n)
	  (setq found t)
	(setq i (1+ i))))
    (substring str 0 i)))
