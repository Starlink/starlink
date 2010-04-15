;;; Starfort mode for GNU Emacs

(defvar starfort-do-indent 3
  "*Extra indentation applied to `do' blocks.")

(defvar starfort-if-indent 3
  "*Extra indentation applied to `if' blocks.")

(defvar starfort-continuation-indent 5
  "*Extra indentation applied to `continuation' lines.")

(defvar starfort-comment-indent-style 'fixed
  "*nil forces comment lines not to be touched,
'fixed produces fixed comment indentation to comment-column,
and 'relative indents to current starfort indentation plus comment-column.")

(defvar starfort-comment-line-column 3
  "*Indentation for text in comment lines.")

(defvar comment-line-start "* "
  "*Delimiter inserted to start new full-line comment.")

(defvar comment-line-start-skip nil
  "*Regexp to match the start of a full-line comment.")

(defvar starfort-minimum-statement-indent 6
  "*Minimum indentation for starfort statements.")

;; Note that this is documented in the v18 manuals as being a string
;; of length one rather than a single character.
;; The code in this file accepts either format for compatibility.
(defvar starfort-comment-indent-char ?
  "*Character to be inserted for Starfort comment indentation.
Normally a space.")

(defvar starfort-line-number-indent 1
  "*Maximum indentation for Starfort line numbers.
5 means right-justify them within their five-column field.")

(defvar starfort-check-all-num-for-matching-do nil
  "*Non-nil causes all numbered lines to be treated as possible do-loop ends.")

(defvar starfort-continuation-char ?:
  "*Character which is inserted in column 5 by \\[starfort-split-line]
to begin a continuation line.  Normally $.")

(defvar starfort-comment-region "c$$$"
  "*String inserted by \\[starfort-comment-region] at start of each line in region.")

(defvar starfort-electric-line-number t
  "*Non-nil causes line number digits to be moved to the correct column as typed.")

(defvar starfort-startup-message t
  "*Non-nil displays a startup message when starfort-mode is first called.")

(defvar starfort-column-ruler
  (concat "0   4 6  10        20        30        40        50        60        70\n"
	  "[   ]|{   |    |    |    |    |    |    |    |    |    |    |    |    |}\n")
  "*String displayed above current line by \\[starfort-column-ruler].")

(defconst starfort-mode-version "prototype")

(defvar starfort-mode-syntax-table nil
  "Syntax table in use in starfort-mode buffers.")

(defvar starfort-initial-string "      [program_module]\n")

(if starfort-mode-syntax-table
    ()
  (setq starfort-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?_ "w" starfort-mode-syntax-table)
  (modify-syntax-entry ?\; "." starfort-mode-syntax-table)
  (modify-syntax-entry ?\: "." starfort-mode-syntax-table)
  (modify-syntax-entry ?+ "." starfort-mode-syntax-table)
  (modify-syntax-entry ?= "." starfort-mode-syntax-table)
  (modify-syntax-entry ?- "." starfort-mode-syntax-table)
  (modify-syntax-entry ?* "." starfort-mode-syntax-table)
  (modify-syntax-entry ?/ "." starfort-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" starfort-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" starfort-mode-syntax-table)
  (modify-syntax-entry ?\\ "/" starfort-mode-syntax-table)
  (modify-syntax-entry ?. "." starfort-mode-syntax-table)
  (modify-syntax-entry ?, "." starfort-mode-syntax-table)
  (modify-syntax-entry ?\n ">" starfort-mode-syntax-table))

(defvar starfort-mode-map ()
  "Keymap used in starfort mode.")

(if starfort-mode-map
    ()
  (setq starfort-mode-map (make-sparse-keymap))
  (define-key starfort-mode-map ";" 'starfort-abbrev-start)
  (define-key starfort-mode-map "\C-c;" 'starfort-comment-region)
  (define-key starfort-mode-map "\e\C-a" 'beginning-of-starfort-subprogram)
  (define-key starfort-mode-map "\e\C-e" 'end-of-starfort-subprogram)
  (define-key starfort-mode-map "\e;" 'starfort-indent-comment)
  (define-key starfort-mode-map "\e\C-h" 'mark-starfort-subprogram)
  (define-key starfort-mode-map "\e\n" 'starfort-split-line)
  (define-key starfort-mode-map "\e\C-q" 'starfort-indent-subprogram)
  (define-key starfort-mode-map "\C-c\C-w" 'starfort-window-create)
  (define-key starfort-mode-map "\C-c\C-r" 'starfort-column-ruler)
  (define-key starfort-mode-map "\C-c\C-p" 'starfort-previous-statement)
  (define-key starfort-mode-map "\C-c\C-n" 'starfort-next-statement)
;  (define-key starfort-mode-map "\t" 'starfort-indent-line)
;  (define-key starfort-mode-map "0" 'starfort-electric-line-number)
;  (define-key starfort-mode-map "1" 'starfort-electric-line-number)
;  (define-key starfort-mode-map "2" 'starfort-electric-line-number)
;  (define-key starfort-mode-map "3" 'starfort-electric-line-number)
;  (define-key starfort-mode-map "4" 'starfort-electric-line-number)
;  (define-key starfort-mode-map "5" 'starfort-electric-line-number)
;  (define-key starfort-mode-map "6" 'starfort-electric-line-number)
;  (define-key starfort-mode-map "7" 'starfort-electric-line-number)
;  (define-key starfort-mode-map "8" 'starfort-electric-line-number)
;  (define-key starfort-mode-map "9" 'starfort-electric-line-number)
)

(defvar starfort-mode-abbrev-table nil)
(if starfort-mode-abbrev-table
    ()
  (define-abbrev-table 'starfort-mode-abbrev-table ())
  (let ((abbrevs-changed nil))
    (define-abbrev starfort-mode-abbrev-table  ";b"   "byte" nil)
    (define-abbrev starfort-mode-abbrev-table  ";ch"  "character" nil)
    (define-abbrev starfort-mode-abbrev-table  ";cl"  "close" nil)
    (define-abbrev starfort-mode-abbrev-table  ";c"   "continue" nil)
    (define-abbrev starfort-mode-abbrev-table  ";cm"  "common" nil)
    (define-abbrev starfort-mode-abbrev-table  ";cx"  "complex" nil)
    (define-abbrev starfort-mode-abbrev-table  ";di"  "dimension" nil)
    (define-abbrev starfort-mode-abbrev-table  ";do"  "double" nil)
    (define-abbrev starfort-mode-abbrev-table  ";dc"  "double complex" nil)
    (define-abbrev starfort-mode-abbrev-table  ";dp"  "double precision" nil)
    (define-abbrev starfort-mode-abbrev-table  ";dw"  "do while" nil)
    (define-abbrev starfort-mode-abbrev-table  ";e"   "else" nil)
    (define-abbrev starfort-mode-abbrev-table  ";ed"  "enddo" nil)
    (define-abbrev starfort-mode-abbrev-table  ";el"  "elseif" nil)
    (define-abbrev starfort-mode-abbrev-table  ";en"  "endif" nil)
    (define-abbrev starfort-mode-abbrev-table  ";eq"  "equivalence" nil)
    (define-abbrev starfort-mode-abbrev-table  ";ex"  "external" nil)
    (define-abbrev starfort-mode-abbrev-table  ";ey"  "entry" nil)
    (define-abbrev starfort-mode-abbrev-table  ";f"   "format" nil)
    (define-abbrev starfort-mode-abbrev-table  ";fu"  "function" nil)
    (define-abbrev starfort-mode-abbrev-table  ";g"   "goto" nil)
    (define-abbrev starfort-mode-abbrev-table  ";im"  "implicit" nil)
    (define-abbrev starfort-mode-abbrev-table  ";ib"  "implicit byte" nil)
    (define-abbrev starfort-mode-abbrev-table  ";ic"  "implicit complex" nil)
    (define-abbrev starfort-mode-abbrev-table  ";ich" "implicit character" nil)
    (define-abbrev starfort-mode-abbrev-table  ";ii"  "implicit integer" nil)
    (define-abbrev starfort-mode-abbrev-table  ";il"  "implicit logical" nil)
    (define-abbrev starfort-mode-abbrev-table  ";ir"  "implicit real" nil)
    (define-abbrev starfort-mode-abbrev-table  ";inc" "include" nil)
    (define-abbrev starfort-mode-abbrev-table  ";in"  "integer" nil)
    (define-abbrev starfort-mode-abbrev-table  ";intr" "intrinsic" nil)
    (define-abbrev starfort-mode-abbrev-table  ";l"   "logical" nil)
    (define-abbrev starfort-mode-abbrev-table  ";op"  "open" nil)
    (define-abbrev starfort-mode-abbrev-table  ";pa"  "parameter" nil)
    (define-abbrev starfort-mode-abbrev-table  ";pr"  "program" nil)
    (define-abbrev starfort-mode-abbrev-table  ";p"   "print" nil)
    (define-abbrev starfort-mode-abbrev-table  ";re"  "real" nil)
    (define-abbrev starfort-mode-abbrev-table  ";r"   "read" nil)
    (define-abbrev starfort-mode-abbrev-table  ";rt"  "return" nil)
    (define-abbrev starfort-mode-abbrev-table  ";rw"  "rewind" nil)
    (define-abbrev starfort-mode-abbrev-table  ";s"   "stop" nil)
    (define-abbrev starfort-mode-abbrev-table  ";su"  "subroutine" nil)
    (define-abbrev starfort-mode-abbrev-table  ";ty"  "type" nil)
    (define-abbrev starfort-mode-abbrev-table  ";w"   "write" nil)))

;;;###autoload
(defun starfort-mode ()
  "Major mode for editing starfort code.
Tab indents the current starfort line correctly.
`do' statements must not share a common `continue'.

Type `;?' or `;\\[help-command]' to display a list of built-in abbrevs for Starfort keywords.

Variables controlling indentation style and extra features:

 comment-start
    Normally nil in Starfort mode.  If you want to use comments
    starting with `!', set this to the string \"!\".
 starfort-do-indent
    Extra indentation within do blocks.  (default 3)
 starfort-if-indent
    Extra indentation within if blocks.  (default 3)
 starfort-continuation-indent
    Extra indentation appled to continuation statements.  (default 5)
 starfort-comment-line-column
    Amount of indentation for text within full-line comments. (default 6)
 starfort-comment-indent-style
    nil    means don't change indentation of text in full-line comments,
    fixed  means indent that text at column starfort-comment-line-column
    relative  means indent at starfort-comment-line-column beyond the
 	      indentation for a line of code.
    Default value is fixed.
 starfort-comment-indent-char
    Character to be inserted instead of space for full-line comment
    indentation.  (default is a space)
 starfort-minimum-statement-indent
    Minimum indentation for starfort statements. (default 6)
 starfort-line-number-indent
    Maximum indentation for line numbers.  A line number will get
    less than this much indentation if necessary to avoid reaching
    column 5.  (default 1)
 starfort-check-all-num-for-matching-do
    Non-nil causes all numbered lines to be treated as possible 'continue'
    statements.  (default nil)
 starfort-continuation-char
    character to be inserted in column 5 of a continuation line.
    (default $)
 starfort-comment-region
    String inserted by \\[starfort-comment-region] at start of each line in
    region.  (default \"c$$$\")
 starfort-electric-line-number
    Non-nil causes line number digits to be moved to the correct column
    as typed.  (default t)
 starfort-startup-message
    Set to nil to inhibit message first time starfort-mode is used.

Turning on Starfort mode calls the value of the variable starfort-mode-hook
with no args, if that value is non-nil.
\\{starfort-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (if starfort-startup-message
      (message "Starfort version %s.  Bugs to rfws@star.rl.ac.uk" starfort-mode-version))
  (setq starfort-startup-message nil)
  (setq local-abbrev-table starfort-mode-abbrev-table)
  (set-syntax-table starfort-mode-syntax-table)

;;; Indent a line for starfort mode.
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'starfort-indent-line)

  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'starfort-comment-hook)
  (make-local-variable 'comment-line-start-skip)
  (setq comment-line-start-skip "^[Cc*][^ \t\n]*[ \t]*") ;[^ \t\n]* handles comment strings such as c$$$
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "![ \t]*")
  (make-local-variable 'comment-start)
  (setq comment-start "!")
  (make-local-variable 'abbrev-all-caps)
  (setq abbrev-all-caps t)
  (make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode nil)
  (use-local-map starfort-mode-map)
  (setq mode-name "Starfort")
  (setq major-mode 'starfort-mode)

;;; RFWS additions...
  (require 'env)
  (make-local-variable 'edstar-placeholder-table)
  (setq edstar-placeholder-table 'starfort-mode-placeholder-table)
  (make-local-variable 'edstar-token-table)
  (setq edstar-token-table 'starfort-mode-token-table)
  (make-local-variable 'edstar-helpkey-table)
  (setq edstar-helpkey-table 'starfort-mode-helpkey-table)

  (make-local-variable 'edstar-initial-string)
  (setq edstar-initial-string starfort-initial-string) ; Initial buffer string
  (setq fill-column 71)      ; Wrap at 72 columns
  (setq truncate-lines t)    ; Truncate lines which are too long on screen
  (make-local-variable 'file-precious-flag) ; Protect files against I/O errors
  (setq file-precious-flag t)
  (if window-system
      (edstar-enable-screen-limiting 72))


;;; Set up compile command.
  (make-local-variable 'compile-command)
  (setq compile-command (concat
			 (or (getenv "EDSTAR_FC") (getenv "FC") "fort77")
			 " "
			 (let
			     ((fflags
			       (or (getenv "EDSTAR_FFLAGS")
				   (let ((fflags (getenv "FFLAGS")))
				     (if fflags (concat "-c " fflags) "-c")))))
			   (if (and fflags (not (string= fflags "")))
			       (concat fflags " ") ""))
			 (buffer-file-name)))

;;; Turn on auto fill mode for this buffer.
  (make-local-variable 'auto-fill-function)
  (auto-fill-mode t)

;;; Override the do-auto-fill virtual function for this buffer.
  (make-local-variable 'do-auto-fill)
  (edstar-override do-auto-fill ()
                   (starfort-do-auto-fill))

;;; Override the edstar-breakline function for this buffer.
  (make-local-variable 'edstar-breakline)
  (edstar-override edstar-breakline (&optional arg)
                   (starfort-breakline arg))

;;; Over-ride the edstar-self-insert virtual function for this buffer.
  (make-local-variable 'edstar-self-insert)
  (edstar-override edstar-self-insert (&optional arg)
                   (starfort-self-insert arg))

;;; Run starfort mode hooks.
  (run-hooks 'starfort-mode-hook))

;;; Self-insert function for starfort mode.
;;;
;;;###autoload
(defun starfort-self-insert (&optional arg)
  (interactive)
  (let ((char (or arg last-command-char)))
    (if (and (not arg) edstar-auto-place-start (= last-command-char ? ))
        (edstar-expand-auto-place))
    (save-excursion
      (setq place (edstar-in-place)))
    (if place
;;; NB this is wrong - see original virtual function for fix.
        (if (>= (point) (cdr (assq 'name-beginning place)))
            (edstar-expand-place t)))
    (if (and (>= char ?0) (<= char ?9))
        (starfort-electric-line-number char)
      (if arg
          (insert arg)
        (self-insert-command 1)))))

(defun starfort-comment-hook ()
  (save-excursion
    (skip-chars-backward " \t")
    (max (+ 1 (current-column))
	 comment-column)))

(defun starfort-indent-comment ()
  "Align or create comment on current line.
Existing comments of all types are recognized and aligned.
If the line has no comment, a side-by-side comment is inserted and aligned
if the value of  comment-start  is not nil.
Otherwise, a separate-line comment is inserted, on this line
or on a new line inserted before this line if this line is not blank."
  (interactive)
  (beginning-of-line)
  ;; Recognize existing comments of either kind.
  (cond ((looking-at comment-line-start-skip)
	 (starfort-indent-line))
	((re-search-forward comment-start-skip
			    (save-excursion (end-of-line) (point)) t)
	 (indent-for-comment))
	;; No existing comment.
	;; If side-by-side comments are defined, insert one,
	;; unless line is now blank.
	((and comment-start (not (looking-at "^[ \t]*$")))
	 (end-of-line)
	 (delete-horizontal-space)
	 (indent-to (starfort-comment-hook))
	 (insert comment-start))
	;; Else insert separate-line comment, making a new line if nec.
	(t
	 (if (looking-at "^[ \t]*$")
	     (delete-horizontal-space)
	   (beginning-of-line)
	   (insert "\n")
	   (forward-char -1))
	 (insert comment-line-start)
	 (insert-char (if (stringp starfort-comment-indent-char)
			  (aref starfort-comment-indent-char 0)
			  starfort-comment-indent-char)
		      (- (calculate-starfort-indent) (current-column))))))

(defun starfort-comment-region (beg-region end-region arg)
  "Comments every line in the region.
Puts starfort-comment-region at the beginning of every line in the region.
BEG-REGION and END-REGION are args which specify the region boundaries.
With non-nil ARG, uncomments the region."
  (interactive "*r\nP")
  (let ((end-region-mark (make-marker)) (save-point (point-marker)))
    (set-marker end-region-mark end-region)
    (goto-char beg-region)
    (beginning-of-line)
    (if (not arg)			;comment the region
	(progn (insert starfort-comment-region)
	       (while (and  (= (forward-line 1) 0)
			    (< (point) end-region-mark))
		 (insert starfort-comment-region)))
      (let ((com (regexp-quote starfort-comment-region))) ;uncomment the region
	(if (looking-at com)
	    (delete-region (point) (match-end 0)))
	(while (and  (= (forward-line 1) 0)
		     (< (point) end-region-mark))
	  (if (looking-at com)
	      (delete-region (point) (match-end 0))))))
    (goto-char save-point)
    (set-marker end-region-mark nil)
    (set-marker save-point nil)))

(defun starfort-abbrev-start ()
  "Typing \";\\[help-command]\" or \";?\" lists all the starfort abbrevs.
Any other key combination is executed normally." ;\\[help-command] is just a way to print the value of the variable help-char.
  (interactive)
  (let (c)
    (insert last-command-char)
    (if (or (= (setq c (read-char)) ??)	;insert char if not equal to `?'
	    (= c help-char))
	(starfort-abbrev-help)
      (setq unread-command-char c))))

(defun starfort-abbrev-help ()
  "List the currently defined abbrevs in Starfort mode."
  (interactive)
  (message "Listing abbrev table...")
  (require 'abbrevlist)
  (list-one-abbrev-table starfort-mode-abbrev-table "*Help*")
  (message "Listing abbrev table...done"))

(defun starfort-column-ruler ()
  "Inserts a column ruler momentarily above current line, till next keystroke.
The ruler is defined by the value of starfort-column-ruler.
The key typed is executed unless it is SPC."
  (interactive)
  (momentary-string-display
   starfort-column-ruler (save-excursion (beginning-of-line) (point))
   nil "Type SPC or any command to erase ruler."))

(defun starfort-window-create ()
  "Makes the window 72 columns wide."
  (interactive)
  (let ((window-min-width 2))
    (split-window-horizontally 73))
  (other-window 1)
  (switch-to-buffer " starfort-window-extra" t)
  (select-window (previous-window)))

(defun starfort-split-line ()
  "Break line at point and insert continuation marker and alignment."
  (interactive)
  (delete-horizontal-space)
  (if (save-excursion (beginning-of-line) (looking-at comment-line-start-skip))
      (insert "\n" comment-line-start " ")
    (insert "\n " starfort-continuation-char))
  (starfort-indent-line))

(defun delete-horizontal-regexp (chars)
  "Delete all characters in CHARS around point.
CHARS is like the inside of a [...] in a regular expression
except that ] is never special and \ quotes ^, - or \."
  (interactive "*s")
  (skip-chars-backward chars)
  (delete-region (point) (progn (skip-chars-forward chars) (point))))

(defun starfort-electric-line-number (arg)
  "Self insert, but if part of a Starfort line number indent it automatically."
  (interactive "P")
  (if (not starfort-electric-line-number)
      (self-insert-command arg)
    (if (or (save-excursion (re-search-backward "[^ \t0-9]"
						(save-excursion
						  (beginning-of-line)
						  (point))
						t)) ;not a line number
	    (looking-at "[0-9]"))		;within a line number
	(insert (or arg last-command-char))
      (skip-chars-backward " \t")
      (insert (or arg last-command-char))
      (starfort-indent-line))))

(defun beginning-of-starfort-subprogram ()
  "Moves point to the beginning of the current starfort subprogram."
  (interactive)
  (let ((case-fold-search t))
    (beginning-of-line -1)
    (re-search-backward "^[ \t0-9]*end\\b[ \t]*[^ \t=(a-z]" nil 'move)
    (if (looking-at "^[ \t0-9]*end\\b[ \t]*[^ \t=(a-z]")
	(forward-line 1))))

(defun end-of-starfort-subprogram ()
  "Moves point to the end of the current starfort subprogram."
  (interactive)
  (let ((case-fold-search t))
    (beginning-of-line 2)
    (re-search-forward "^[ \t0-9]*end\\b[ \t]*[^ \t=(a-z]" nil 'move)
    (goto-char (match-beginning 0))
    (forward-line 1)))

(defun mark-starfort-subprogram ()
  "Put mark at end of starfort subprogram, point at beginning.
The marks are pushed."
  (interactive)
  (end-of-starfort-subprogram)
  (push-mark (point))
  (beginning-of-starfort-subprogram))

(defun starfort-previous-statement ()
  "Moves point to beginning of the previous starfort statement.
Returns 'first-statement if that statement is the first
non-comment Starfort statement in the file, and nil otherwise."
  (interactive)
  (let (not-first-statement continue-test)
    (beginning-of-line)
    (setq continue-test
	  (or (looking-at
	        (concat "[ \t]*" (regexp-quote (char-to-string
						 starfort-continuation-char))))
	      (looking-at "     [^ 0\n]")))
    (while (and (setq not-first-statement (= (forward-line -1) 0))
		(or (looking-at comment-line-start-skip)
		    (looking-at "[ \t]*$")
		    (looking-at "     [^ 0\n]")
		    (looking-at (concat "[ \t]*"  comment-start-skip)))))
    (cond ((and continue-test
		(not not-first-statement))
	   (message "Incomplete continuation statement."))
	  (continue-test
	   (starfort-previous-statement))
	  ((not not-first-statement)
	   'first-statement))))

(defun starfort-next-statement ()
  "Moves point to beginning of the next starfort statement.
 Returns 'last-statement if that statement is the last
 non-comment Starfort statement in the file, and nil otherwise."
  (interactive)
  (let (not-last-statement)
    (beginning-of-line)
    (while (and (setq not-last-statement (= (forward-line 1) 0))
 		(or (looking-at comment-line-start-skip)
 		    (looking-at "[ \t]*$")
 		    (looking-at "     [^ 0\n]")
 		    (looking-at (concat "[ \t]*"  comment-start-skip)))))
    (if (not not-last-statement)
 	'last-statement)))

(defun starfort-indent-line ()
  "Indents current starfort line based on its contents and on previous lines."
  (interactive)
  (let ((cfi (calculate-starfort-indent)))
    (save-excursion
      (beginning-of-line)
      (if (or (not (= cfi (starfort-current-line-indentation)))
	      (and (re-search-forward "^[ \t]*[0-9]+" (+ (point) 4) t)
		   (not (starfort-line-number-indented-correctly-p))))
	  (starfort-indent-to-column cfi)
	(beginning-of-line)
	(if (re-search-forward comment-start-skip
			       (save-excursion (end-of-line) (point)) 'move)
	    (starfort-indent-comment))))
    ;; Never leave point in left margin.
    (if (< (current-column) cfi)
	(move-to-column cfi))))

(defun starfort-indent-subprogram ()
  "Properly indents the Starfort subprogram which contains point."
  (interactive)
  (save-excursion
    (mark-starfort-subprogram)
    (message "Indenting subprogram...")
    (indent-region (point) (mark) nil))
  (message "Indenting subprogram...done."))

(defun calculate-starfort-indent ()
  "Calculates the starfort indent column based on previous lines."
  (let (icol (iscom 0) first-statement (case-fold-search t))
    (save-excursion
      (setq first-statement (starfort-previous-statement))
      (if first-statement
	  (setq icol starfort-minimum-statement-indent)
	(progn
	  (if (= (point) (point-min))
	      (setq icol starfort-minimum-statement-indent)
	    (setq icol (starfort-current-line-indentation)))
	  (skip-chars-forward " \t0-9")
	  (cond ((looking-at "if[ \t]*(")
		 (if (or (looking-at ".*)[ \t]*then\\b[ \t]*[^ \t(=a-z0-9]")
			 (let (then-test)	;multi-line if-then
			   (while (and (= (forward-line 1) 0) ;search forward for then
				       (looking-at "     [^ 0]")
				       (not (setq then-test (looking-at ".*then\\b[ \t]*[^ \t(=a-z0-9]")))))
			   then-test))
		     (setq icol (+ icol starfort-if-indent))))
		((looking-at "\\(else\\|elseif\\)\\b")
		 (setq icol (+ icol starfort-if-indent)))
		((looking-at "do\\b")
		 (setq icol (+ icol starfort-do-indent)))))))
    (save-excursion
      (beginning-of-line)
      (cond ((looking-at "[ \t]*$"))
	    ((looking-at comment-line-start-skip) ; RFWS - changes to
             (setq iscom t)                       ; align with previous comment
	     (if (= (point) (point-min))          ; line
		 (setq icol starfort-comment-line-column)
	       (save-excursion
		 (forward-line -1)
		 (if (looking-at comment-line-start-skip)
		     (setq icol (starfort-current-line-indentation))
		   (cond ((eq starfort-comment-indent-style 'relative)
			  (setq icol (+ icol starfort-comment-line-column)))
			 ((eq starfort-comment-indent-style 'fixed)
			  (setq icol starfort-comment-line-column)))))))
	    ((or (looking-at (concat "[ \t]*"
				     (regexp-quote (char-to-string starfort-continuation-char))))
		 (looking-at "     [^ 0\n]"))
	     (setq icol (+ icol starfort-continuation-indent)))
	    (first-statement)
	    ((and starfort-check-all-num-for-matching-do
		  (looking-at "[ \t]*[0-9]+")
		  (starfort-check-for-matching-do))
	     (setq icol (- icol starfort-do-indent)))
	    (t
	     (skip-chars-forward " \t0-9")
	     (cond ((looking-at "end[ \t]*if\\b")
		    (setq icol (- icol starfort-if-indent)))
		   ((looking-at "\\(else\\|elseif\\)\\b")
		    (setq icol (- icol starfort-if-indent)))
		   ((and (looking-at "continue\\b")
			 (starfort-check-for-matching-do))
		    (setq icol (- icol starfort-do-indent)))
		   ((looking-at "end[ \t]*do\\b")
		    (setq icol (- icol starfort-do-indent)))
		   ((and (looking-at "end\\b[ \t]*[^ \t=(a-z]")
			 (not (= icol starfort-minimum-statement-indent)))
 		    (message "Warning: `end' not in column %d.  Probably an unclosed block." starfort-minimum-statement-indent))))))
    (if iscom icol (max starfort-minimum-statement-indent icol))))

(defun starfort-current-line-indentation ()
  "Indentation of current line, ignoring Starfort line number or continuation.
This is the column position of the first non-whitespace character
aside from the line number and/or column 5 line-continuation character.
For comment lines, returns indentation of the first
non-indentation text within the comment."
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at comment-line-start-skip)
	   (goto-char (match-end 0))
	   (skip-chars-forward
	     (if (stringp starfort-comment-indent-char)
		 starfort-comment-indent-char
	         (char-to-string starfort-comment-indent-char))))
	  ((looking-at "     [^ 0\n]")
	   (goto-char (match-end 0)))
	  (t
	   ;; Move past line number.
	   (move-to-column 5)))
    ;; Move past whitespace.
    (skip-chars-forward " \t")
    (current-column)))

(defun starfort-indent-to-column (col)
  "Indents current line with spaces to column COL.
notes: 1) A non-zero/non-blank character in column 5 indicates a continuation
          line, and this continuation character is retained on indentation;
       2) If starfort-continuation-char is the first non-whitespace character,
          this is a continuation line;
       3) A non-continuation line which has a number as the first
          non-whitespace character is a numbered line."
  (save-excursion
    (beginning-of-line)
    (if (looking-at comment-line-start-skip)
	(if starfort-comment-indent-style
	    (let ((char (if (stringp starfort-comment-indent-char)
			    (aref starfort-comment-indent-char 0)
			    starfort-comment-indent-char)))
	      (goto-char (match-end 0))
	      (delete-horizontal-regexp (concat " \t" (char-to-string char)))
	      (insert-char char (- col (current-column)))))
      (if (looking-at "     [^ 0\n]")
	  (forward-char 6)
	(delete-horizontal-space)
	;; Put line number in columns 0-4
	;; or put continuation character in column 5.
	(cond ((eobp))
	      ((= (following-char) starfort-continuation-char)
	       (indent-to 5)
	       (forward-char 1))
	      ((looking-at "[0-9]+")
	       (let ((extra-space (- 5 (- (match-end 0) (point)))))
		 (if (< extra-space 0)
		     (message "Warning: line number exceeds 5-digit limit.")
		   (indent-to (min starfort-line-number-indent extra-space))))
	       (skip-chars-forward "0-9"))))
      ;; Point is now after any continuation character or line number.
      ;; Put body of statement where specified.
      (delete-horizontal-space)
      (indent-to col)
      ;; Indent any comment following code on the same line.
      (if (re-search-forward comment-start-skip
			     (save-excursion (end-of-line) (point)) t)
	  (progn (goto-char (match-beginning 0))
		 (if (not (= (current-column) (starfort-comment-hook)))
		     (progn (delete-horizontal-space)
			    (indent-to (starfort-comment-hook)))))))))

(defun starfort-line-number-indented-correctly-p ()
  "Return t if current line's line number is correctly indente.
Do not call if there is no line number."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (and (<= (current-column) starfort-line-number-indent)
	 (or (= (current-column) starfort-line-number-indent)
	     (progn (skip-chars-forward "0-9")
		    (= (current-column) 5))))))

(defun starfort-check-for-matching-do ()
  "When called from a numbered statement, returns t
 if matching 'do' is found, and nil otherwise."
  (let (charnum
	(case-fold-search t))
    (save-excursion
      (beginning-of-line)
      (if (looking-at "[ \t]*[0-9]+")
	  (progn
	    (skip-chars-forward " \t")
	    (skip-chars-forward "0") ;skip past leading zeros
	    (setq charnum (buffer-substring (point)
					    (progn (skip-chars-forward "0-9")
						   (point))))
	    (beginning-of-line)
	    (and (re-search-backward
		  (concat "\\(^[ \t0-9]*end\\b[ \t]*[^ \t=(a-z]\\)\\|\\(^[ \t0-9]*do[ \t]*0*"
			  charnum "\\b\\)\\|\\(^[ \t]*0*" charnum "\\b\\)")
		  nil t)
		 (looking-at (concat "^[ \t0-9]*do[ \t]*0*" charnum))))))))


