
;;;### (autoloads (edstar-abs-file) "abs_file" "abs_file.el" (13525 61248))
;;;### (autoloads (edstar-align-trailing) "align_tr" "align_tr.el" (13522 8677))
;;;### (autoloads (starfort-breakline starfort-newline starfort-do-auto-fill starfort-break-line starfort-line-break-point starfort-backup-to-break starfort-line-break-context) "break" "break.el" (13540 24973))
;;;### (autoloads (edstar-choose-from-menu) "choose_f" "choose_f.el" (13521 46034))
;;;### (autoloads (edstar-insert-comment-line) "comment1" "comment1.el" (13535 14039))
;;;### (autoloads (starfort-break-line starfort-line-break-context) "comment2" "comment2.el" (13539 13945))
;;;### (autoloads (backward-error forward-error com) "compile" "compile.el" (13521 46034))
;;;### (autoloads (edstar-cut-replace edstar-paste edstar-cut-append edstar-cut) "cut" "cut.el" (13521 46034))
;;;### (autoloads (edstar-undelete-line edstar-delete-eol edstar-delete-line edstar-undelete-word edstar-delete-word edstar-undelete-char edstar-delete-char-reverse edstar-delete-char) "delete" "delete.el" (13521 46036))
;;;### (autoloads (edstar-detach) "detach" "detach.el" (13521 46035))
;;;### (autoloads (edstar-set-reverse edstar-set-forward) "directio" "directio.el" (13521 46034))
;;;### (autoloads (starfort-rebuild-env starfort-save-env) "do_env" "do_env.el" (13529 45472))
;;;### (autoloads (edstar-dump-symbol edstar-dump-tables) "dump_tab" "dump_tab.el" (13521 62885))
;;;### (autoloads (quit exit) "exit" "exit.el" (13521 46034))
;;;### (autoloads (edstar-find-file-buffer) "file_buff" "file_buff.el" (13525 63465))
;;;### (autoloads (edstar-file-complete edstar-file-prompt) "file_pro" "file_pro.el" (13521 46034))
;;;### (autoloads (edstar-find) "find" "find.el" (13525 61977))
;;;### (autoloads (edstar-find-keyword-string) "find_key" "find_key.el" (13529 47955))
;;;### (autoloads (edstar-get-desc-string) "get_desc" "get_desc.el" (13521 58842))
;;;### (autoloads (edstar-handle-menu-motion) "handle_m" "handle_m.el" (13521 46034))
;;;### (autoloads (edstar-find-helpkey edstar-help-on-this auto-help) "help" "help.el" (13529 57484))
;;;### (autoloads (edstar-end-learn learn) "learn" "learn.el" (13521 46036))
;;;### (autoloads (edstar-lookup-keyword) "lookup_k" "lookup_k.el" (13529 40957))
;;;### (autoloads (edstar-lookup-place) "lookup_p" "lookup_p.el" (13526 692))
;;;### (autoloads (edstar-make-virtual) "make_virtual" "make_virtual.el" (13540 12760))
;;;### (autoloads (edstar-scroll edstar-scroll-reverse edstar-scroll-forward edstar-goto-bottom edstar-goto-top edstar-goto-word edstar-goto-word-reverse edstar-goto-word-forward edstar-goto-page edstar-goto-eol) "motion" "motion.el" (13521 46034))
;;;### (autoloads (new-helpkey) "new_help" "new_help.el" (13529 38653))
;;;### (autoloads (new-place) "new_plac" "new_plac.el" (13529 38577))
;;;### (autoloads (new-routine) "new_rout" "new_rout.el" (13530 873))
;;;### (autoloads (new-token) "new_toke" "new_toke.el" (13529 37284))
;;;### (autoloads (edstar-override) "override" "override.el" (13540 8893))
;;;### (autoloads (previous-placeholder next-placeholder erase-placeholder expand-indicated move-down move-up move-right move-left edstar-next-line edstar-expand-auto-place edstar-kill-place edstar-best-place edstar-in-place edstar-goto-place edstar-goto-place-string) "place" "place.el" (13540 24363))
;;;### (autoloads (edstar-possible-files) "possible" "possible.el" (13525 52150))
;;;### (autoloads (edstar-check-readonly-dir) "readonly" "readonly.el" (13525 62929))
;;;### (autoloads (edstar-reserve) "reserve" "reserve.el" (13521 46035))
;;;### (autoloads (edstar-save-buffers) "save_buf" "save_buf.el" (13521 46035))
;;;### (autoloads (edstar-enable-screen-limiting) "screen_l" "screen_l.el" (13521 46036))
;;;### (autoloads (repl res) "sdt" "sdt.el" (13525 63341))
;;;### (autoloads (edstar-search-again edstar-search) "search" "search.el" (13521 46035))
;;;### (autoloads (edstar-select-buffer edstar-list-buffers) "select_b" "select_b.el" (13521 46035))
;;;### (autoloads (edstar-selection-isset edstar-selection-end edstar-selection-beginning edstar-unset-selection edstar-set-selection) "selection" "selection.el" (13521 46035))
;;;### (autoloads (edstar-split-path) "split_pa" "split_pa.el" (13525 51647))
;;;### (autoloads (starfort-self-insert starfort-mode) "starfort" "starfort.el" (13540 25231))
;;;### (autoloads (edstar-toggle-case) "toggle_c" "toggle_c.el" (13521 46035))
;;;### (autoloads (edstar-toggle-windows) "toggle_w" "toggle_w.el" (13521 46035))
;;;### (autoloads (edstar-first-line-of-string edstar-insert-duplicate-place edstar-show-place-help edstar-expand-place edstar-expand-token edstar-expand-keyword edstar-expand edstar-insert-expansion-text edstar-find-keyword) "token" "token.el" (13540 24333))
;;;### (autoloads (edstar-virtual) "virtual" "virtual.el" (13540 8893))
;;;### (autoloads (edstar-visit goto-file) "visit" "visit.el" (13521 46035))
;;;### (autoloads (edstar-current-line-in-window) "window" "window.el" (13521 46037))
;;; Generated autoloads from window.el

(autoload (quote edstar-current-line-in-window) "window" nil nil nil)

;;;***

;;; Generated autoloads from visit.el
(defvar edstar-initial-string nil)

(autoload (quote goto-file) "visit" "\

Purpose:
   Visit a file in a buffer, using EDSTAR_PATH to search for it.

Description:
   The function opens a file and reads it into a buffer, making that buffer
   current. In searching for the file, it uses the directory search path
   specified by the EDSTAR_PATH environment variable. If the file is not
   found, a new file is created.

Arguments:
   file
      The name of the file to be visited, as string. If nil is given the file
      will be prompted for.
" t nil)

(autoload (quote edstar-visit) "visit" "\

Purpose:
   Visit a file in a buffer, using EDSTAR_PATH to search for it.

Description:
   The function opens a file and reads it into a buffer, making that buffer
   current. In searching for the file, it uses the directory search path
   specified by the EDSTAR_PATH environment variable. If the file is not
   found, a new file is created.

Arguments:
   file
      The name of the file to be visited, as a string. If nil is given the
      file will be prompted for.

Notes:
   This function is not interactively callable; goto-file provides the user
   callable interface.
" nil nil)

;;;***

;;; Generated autoloads from virtual.el

(autoload (quote edstar-virtual) "virtual" "\
Purpose:
   Define a new \"virtual function\" for emacs.

Synopsis:
   (edstar-virtual fun args &rest body)

Description:
   This macro allows the creation of virtual functions. These may be used just
   like normal functions (created using defun), except that their lisp
   implementation may later be locally overridden. (This differs from simply
   using defun a second time to re-define the function, since this would
   modify the function permanently, with global scope, and destroy the original
   definition.)

   With a virtual function, the original global definition remains in place
   when the function is overridden and only the local definition (stored in a
   suitable local variable) is changed. This allows the function to be adapted
   for more specialised purposes on, say, a per-buffer basis, without affecting
   its use elsewhere.

   The term \"virtual function\" is object-oriented terminology for a function
   whose interface may be inherited from a base class by a more specialised
   class, but whose implementation may be overridden to provide more
   specialised (and appropriate) behaviour for a particular type of data. The
   use in emacs is very similar and fulfils the same purpose, except that the
   context within which the function is overridden will normally be an editing
   buffer or a (let...) form.

   To override a virtual function, you must create a suitable local variable
   with the same name as the function and then use edstar-override.

Arguments:
   fun
      The name of the virtual function.
   args
      The argument list.
   body
      The body of the function.

Notes:
   - This macro is used exactly like defun and takes the same arguments (but
   see below for differences associated with loading).

   - If using autoloading, you must ensure that any use of edstar-virtual
   is loaded before the virtual function it defines is overridden. If this
   is not done, subsequent autoloading may cause the new definition to be
   over-written.

   - Note that ;;;###autoload can be used to perform autoloading of a virtual
   function, but will result in the function's complete implementation being
   copied into the \"loaddefs.el\" file (unlike defun, which only generates
   an instruction to load the necessary file). This means that \"loaddefs.el\"
   must be updated if the function is changed. The need to do this can be
   minimised, however, if the function's implementation is encapsulated in a
   second (autoloaded) function.
" nil (quote macro))

;;;***

;;; Generated autoloads from token.el
(defvar edstar-indent 3)
(defvar edstar-auto-place-start nil)
(defvar edstar-auto-place-name nil)

(autoload (quote edstar-find-keyword) "token" nil t nil)

(autoload (quote edstar-insert-expansion-text) "token" nil nil nil)

(autoload (quote edstar-expand) "token" nil t nil)

(autoload (quote edstar-expand-keyword) "token" nil t nil)

(autoload (quote edstar-expand-token) "token" nil t nil)

(autoload (quote edstar-expand-place) "token" nil t nil)

(autoload (quote edstar-show-place-help) "token" nil nil nil)

(autoload (quote edstar-insert-duplicate-place) "token" nil nil nil)

(autoload (quote edstar-first-line-of-string) "token" nil nil nil)

;;;***

;;; Generated autoloads from toggle_w.el

(autoload (quote edstar-toggle-windows) "toggle_w" "\

Purpose:
   Toggle the number of visible screen windows between 1 and 2.

Description:
   If there is only one window, then it is split horizontally into two equal
   windows. If more than one is visible, then all except the current window
   is deleted, leaving only a single window.
" t nil)

;;;***

;;; Generated autoloads from toggle_c.el

(autoload (quote edstar-toggle-case) "toggle_c" "\

Purpose:
   Toggle the case (of a character or of the current selection).

Description:
   The function processes either the current character or, if a selection
   is active, all the text in the current selection. It toggles its case,
   changing lower case characters to upper case, and vice versa.

Notes:
   The current selection is cancelled by this function.
" t nil)

;;;***

;;; Generated autoloads from starfort.el

(autoload (quote starfort-mode) "starfort" "\
Major mode for editing starfort code.
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
\\{starfort-mode-map}" t nil)

(autoload (quote starfort-self-insert) "starfort" nil t nil)

;;;***

;;; Generated autoloads from split_pa.el

(autoload (quote edstar-split-path) "split_pa" "\

Purpose:
   Split a directory search path into fields.

Description:
   The function returns a list of strings obtained by splitting the contents of
   a \"path\" string  (which represents a list of directories to be searched)
   into its constituent elements. The \":\" character is recognised as the
   field delimiter within the path.

Arguments:
   path
      A string which holds the path specification to be split. If nil is
      given, the function returns nil.
" nil nil)

;;;***

;;; Generated autoloads from selectio.el

(autoload (quote edstar-set-selection) "selectio" "\

Purpose:
   Set the start of the text selection.

Description:
   The function sets the start of the current text selection to the current
   editing position (point) in the current buffer, overriding any previous
   value. This makes the text selection active, the selected text being that
   lying between the nominated start position and any subsequent editing
   position (point).

Notes:
   -  This function sets the mark for the current buffer to the value of point
   and pushes the previous mark value (if any) on to the mark ring. The status
   of the mark (active or inactive) is subsequently used to identify whether
   a text selection is active.
   -  This function requires \"transient mark mode\" to be set for correct
   operation (which includes highlighting of selected text). It will set this
   mode itself if necessary.
" t nil)

(autoload (quote edstar-unset-selection) "selectio" "\

Purpose:
   Unset the current text selection.

Description:
   The function clears the current text selection, de-selecting any selected
   text.

Notes:
   This function makes the value of mark inactive in the current buffer to
   indicate that there is no current text selection.
" t nil)

(autoload (quote edstar-selection-beginning) "selectio" "\

Purpose:
   Return the beginning of the current text selection.

Description:
   If text is currently selected, the position of the beginning of the
   selection is returned. Otherwise, the current editing position (point) is
   returned. The returned position will not lie outside the accessible region
   of the current buffer.
" nil nil)

(autoload (quote edstar-selection-end) "selectio" "\

Purpose:
   Return the end of the current selection.

Description:
   If text is currently selected, the position of the end of the selection is
   returned. Otherwise, a position one character beyond the current editing
   position (point) is returned. The returned position will not lie outside
   the accessible region of the current buffer.
" nil nil)

(autoload (quote edstar-selection-isset) "selectio" "\

Purpose:
   Determine if text is currently selected.

Description:
   The function returns t if there is currently selected text, nil otherwise.

Notes:
   The status of the mark (active/inactive) is used to determine if a
   selection is set.
" nil nil)

;;;***

;;; Generated autoloads from select_b.el
(defvar edstar-select-one-window nil)

(autoload (quote edstar-list-buffers) "select_b" "\

Purpose:
   List buffers which are currently visiting files.

Description:
   The function creates a buffer called \"*List Buffers*\" in which it lists
   all the buffers which are currently visiting files. The cursor is placed in
   this buffer and \"Buffer Menu\" mode is selected.

Notes:
   This function is the same as buffer-menu except that it only lists buffers
   which are visiting files. It also notes whether only one window was
   previously displayed for subsequent use by the edstar-select-buffer
   function.
" t nil)

(autoload (quote edstar-select-buffer) "select_b" "\

Purpose:
   Select a buffer in Buffer Menu mode.

Description:
   The function selects the buffer identified by the cursor when in Buffer
   Menu mode. It is equivalent to typing \"1\" or \"2\" when in this mode,
   except that it decides whether to display one or 2 windows depending on
   how many windows were displayed prior to the last \"edstar-list-buffers\"
   command.
" t nil)

;;;***

;;; Generated autoloads from search.el
(defvar edstar-search-target "")

(autoload (quote edstar-search) "search" "\

Purpose:
   Search for a string.

Description:
   The function searches for a specified string in the current buffer, starting
   at the current editing position (point) and searching forwards or backwards
   according to the current default direction setting. If the string is found,
   the editing position (point) is moved to the start of it. Otherwise, an
   error message is given.

Arguments
   target (optional)
      The string to be searched for. This is prompted for if not given.

Notes:
   This function saves the target string so that it may be searched for again
   (e.g. using edstar-search-again).
" t nil)

(autoload (quote edstar-search-again) "search" "\

Purpose:
   Repeat a search for a string.

Description:
   The function repeats a search for a string previously searched for using
   edstar-search. It performs the same search operations as that function,
   except that a search string is not given; the previous one is used again.
" t nil)

;;;***

;;; Generated autoloads from sdt.el

(autoload (quote res) "sdt" "\

Purpose:
   Reserve a file from the (sccs) repository.

Description:
   The function reserves a file from the (sccs) repository and reads it into
   a buffer for editing. The command \"sccs edit\" is used for reserving the
   file, unless an alternative command has been specified via tha EDSTAR_RES
   environment variable.

   If a buffer of the same name was previously in use it is over-written, but
   the editing position (point) is preserved.

Arguments:
   file (optional)
      A string giving the name of the file to be reserved (without any
      directory prefix). The file should exist in the SCCS repository and
      should not already be reserved. If this argument is not given, it is
      prompted for. The default is to use the name of the file being visited
      by the current buffer.
" t nil)

(autoload (quote repl) "sdt" "\

Purpose:
   Replace a file in the (sccs) repository.

Description:
   The function replaces a file in the (sccs) repository and kills any buffer
   which was previously visiting the file (the buffer is first saved if
   necessary). The command \"sccs delta\" is used for replacing the file
   unless an alternative has been supplied via the EDSTAR_REPL environment
   variable.

Arguments:
   file (optional)
      A string giving the name of the file to be replaced (without any
      directory prefix). The file should exist in the SCCS repository and
      should be reserved, with a writeable copy in the current default
      directory. If this argument is not given, it is prompted for. The
      default is to use the name of the file being visited by the current
      buffer.
" t nil)

;;;***

;;; Generated autoloads from screen_l.el

(autoload (quote edstar-enable-screen-limiting) "screen_l" nil nil nil)

;;;***

;;; Generated autoloads from save_buf.el

(autoload (quote edstar-save-buffers) "save_buf" "\

Purpose:
   Save all modified buffers.

Description:
   The function writes the contents of all buffers which have been modified
   back to their associated file, if any.
" t nil)

;;;***

;;; Generated autoloads from reserve.el

(autoload (quote edstar-reserve) "reserve" nil t nil)

;;;***

;;; Generated autoloads from readonly.el

(autoload (quote edstar-check-readonly-dir) "readonly" "\

Purpose:
   Make buffers visiting readonly directories readonly.

Description:
   The function tests whether the current buffer is visiting a file which
   resides in one of the directories appearing in the list given by the
   translation of the environment variable EDSTAR_READONLY (as stored in the
   edstar-readonly list). If it is, the buffer is made readonly.
" nil nil)

;;;***

;;; Generated autoloads from possible.el

(autoload (quote edstar-possible-files) "possible" "\

Purpose:
   Return an alist of all files accessible via EDSTAR_PATH.

Description:
   The function returns an alist of the names of all files which are currently
   accessible in directories on the EDSTAR_PATH search path (it is intended for
   use in file name completion). The returned alist contains file names without
   a directory prefix, without duplicates and in alphabetical order. Each file
   name is associated with the symbol t. Certain files are omitted from the
   alist if they are not suitable for editing (see \"File Exclusion\" for
   details).

Arguments:
   prefix
      If this string argument is non-nil, only those file names which start
      with this prefix will be considered (if not otherwise excluded).
      Otherwise, all file names not beginning with \".\" will be considered.

File Exclusion:
   Certain files are excluded from the returned alist if they are considered
   unsuitable for editing. By default, all files with file type extensions
   of \".o\", \".a\", \".tar\" and \".Z\" are excluded. This choice may be
   overridden by defining the environment variable EDSTAR_IGNORE_FILES to be
   a regular expression which matches the file type extension (excluding the
   \".\") of any files to be excluded. Files whose names begin with \".\" are
   always excluded.
" nil nil)

;;;***

;;; Generated autoloads from place.el
(defvar edstar-placeholder-begin "{")
(defvar edstar-placeholder-end "}")
(defvar edstar-placeholder-begin-opt "[")
(defvar edstar-placeholder-end-opt "]")
(defvar edstar-placeholder-dupe "...")
(defvar edstar-placeholder-chars (concat "[" (regexp-quote " A-Za-z0-9~`@#$%^&*()_+=|\\:;\"'<,>.?/\\-") "]"))

(autoload (quote edstar-goto-place-string) "place" nil t nil)

(autoload (quote edstar-goto-place) "place" nil nil nil)

(autoload (quote edstar-in-place) "place" nil t nil)

(autoload (quote edstar-best-place) "place" nil t nil)

(autoload (quote edstar-kill-place) "place" nil t nil)

(edstar-virtual edstar-self-insert (&optional arg) (interactive) (let ((place nil)) (if (and (not arg) edstar-auto-place-start (= last-command-char 32)) (edstar-expand-auto-place)) (save-excursion (setq place (edstar-in-place))) (if (and place (> (point) (cdr (assq (quote beginning) place)))) (edstar-expand-place t)) (if arg (insert arg) (self-insert-command 1))))

(substitute-key-definition (quote self-insert-command) (quote edstar-self-insert) global-map)

(autoload (quote edstar-expand-auto-place) "place" nil nil nil)

(autoload (quote edstar-next-line) "place" nil t nil)

(autoload (quote move-left) "place" nil t nil)

(autoload (quote move-right) "place" nil t nil)
(defvar edstar-goal-column nil)

(autoload (quote move-up) "place" nil t nil)

(autoload (quote move-down) "place" nil t nil)

(autoload (quote expand-indicated) "place" nil t nil)

(autoload (quote erase-placeholder) "place" nil t nil)

(autoload (quote next-placeholder) "place" nil t nil)

(autoload (quote previous-placeholder) "place" nil t nil)

;;;***

;;; Generated autoloads from override.el

(autoload (quote edstar-override) "override" "\
Purpose:
   Override a \"virtual function\".

Synopsis:
   (edstar-override fun args &rest body)

Description:
   This macro allows a virtual function created with edstar-virtual (q.v.) to
   be overridden by giving it a new implementation (the calling interface
   remains the same).

   The usual way of using this macro is first to create a local version of a
   variable with the same name as the virtual function (either using let or
   make-local-variable) and then to provide a new local implementation of the
   function with edstar-override. The new implementation will then be in effect
   wherever the local variable is within scope, while the previous
   implementation will remain in effect elsewhere.

   Normally, you do not want the original global implementation of a virtual
   function to be affected by edstar-override. However, if you do not create a
   appropriate local variable first, you will be modifying whatever version of
   the variable is within scope at that point. If this is the global version,
   then the original implementation will be destroyed. This is a legitimate
   use of edstar-override.

Arguments:
   fun
      The name of the virtual function to be overridden.
   args
      The argument list. This should match the argument list given in the
      original invocation of edstar-virtual.
   body
      The body of the function. Any documentation string or \"(interactive)\"
      declaration is ignored, since the user-interface is defined by the
      original use of edstar-virtual and is not changed by edstar-override.

Notes:
   - This macro is used like defun and takes the same arguments.

   - You should not normally autoload an invocation of this macro, since its
   purpose is to override a virtual function (normally itself autoloaded) only
   at an appropriate point during execution. Normally, edstar-override will
   be invoked from the body of another function.
" nil (quote macro))

;;;***

;;; Generated autoloads from new_toke.el
(defvar edstar-token-table nil)
(defvar edstar-token-table-size 4095)

(autoload (quote new-token) "new_toke" "\

Purpose:
   Define a new token.

Description:
   The function makes a new entry in the keyword table describing a token.
   If a token of the same name already exists, it is over-written.

Arguments:
   name
      A string containing the name of the token (case insensitive).
   value
      The value of the token. This may be a:

         string
            Interpreted as a keyboard macro used to generate the token
            expansion.
         (other types to be added...)
   prop (optional)
      If given, this should be a alist which defines a list of token
      properties in the form ((pname . pval) (pname . pval) ...), where:

         pname
            Identifies the property (normally a symbol).
         pval
            Is the property's value, whose type should be appropriate to the
            property being defined.
   mode
      The name of the major mode whose table is to receive the new token.
      If not given, the current token table is used if any. Otherwise, a
      token table is created for the current major mode and that is used
      instead.

Returned Value:
   nil
" nil nil)

;;;***

;;; Generated autoloads from new_rout.el

(autoload (quote new-routine) "new_rout" nil nil nil)

;;;***

;;; Generated autoloads from new_plac.el
(defvar edstar-placeholder-table nil)
(defvar edstar-placeholder-table-size 4095)

(autoload (quote new-place) "new_plac" nil nil nil)

;;;***

;;; Generated autoloads from new_help.el
(defvar edstar-helpkey-table nil)
(defvar edstar-helpkey-table-size 4095)

(autoload (quote new-helpkey) "new_help" "\

Purpose:
   Define a new helpkey.

Description:
   The function makes a new entry in the keyword table describing a helpkey.
   If a helpkey of the same name already exists, it is over-written.

Arguments:
   name
      A string containing the name of the helpkey (case insensitive). This
      is a string of text which may appear in an editing buffer and on which
      help information may be requested.
   value
      A list containing a sequence of help keys, as strings, which will be
      used to locate the help information.
   prop (optional)
      If given, this should be a alist which defines a list of helpkey
      properties in the form ((pname . pval) (pname . pval) ...), where:

         pname
            Identifies the property (normally a symbol).
         pval
            Is the property's value, whose type should be appropriate to the
            property being defined.
   mode
      The name of the major mode whose table is to receive the new helpkey.
      If not given, the current helpkey table is used if any. Otherwise, a
      helpkey table is created for the current major mode and that is used
      instead.

Returned Value:
   nil
" nil nil)

;;;***

;;; Generated autoloads from motion.el

(autoload (quote edstar-goto-eol) "motion" "\

Purpose:
   Move to the end of a line.

Description:
   The function moves the editing position (point) to the next end of line,
   in either the forward or reverse direction (according to the current
   direction of motion).
" t nil)

(autoload (quote edstar-goto-page) "motion" "\

Purpose:
   Move to the next page.

Description:
   The function moves the editing position (point) to the start of the
   next page of text, as marked by the next form-feed (^L) character in
   either the forward or reverse direction (according to the current
   direction of motion). If no form-feed characters are found, the editing
   position is moved to the appropriate end of the buffer.

Notes:
   The screen is scrolled as necessary to leave the start of the page near
   the top of the screen, if possible.
" t nil)

(autoload (quote edstar-goto-word-forward) "motion" "\

Purpose:
   Move forward to the next word.

Description:
   The function moves the editing position (point) forward to the start of
   the next word (or other non-word character which is not white space).
" t nil)

(autoload (quote edstar-goto-word-reverse) "motion" "\

Purpose:
   Move backwards to the previous word.

Description:
   The function moves the editing position (point) backwards to the start of
   the previous word (or other non-word character which is not white space).

Notes:
   This function simulates the similar motion command provided by STARLSE.

Bugs:
   Non-word (e.g. punctuation) characters are sometimes missed by this
   function if the editing position is initially positioned immediately
   following them. The regular expression matching behaviour of
   the re-search-backward function needs more investigation in this area.
" t nil)

(autoload (quote edstar-goto-word) "motion" "\

Purpose:
   Move to the next word.

Description:
   The function moves the editing position (point) either forwards or backwards
   to the start of the next (or previous) word, or to any other non-word
   character which is not white space. The default direction of motion is used.

Notes:
   This function simulates the similar motion command provided by STARLSE.
" t nil)

(autoload (quote edstar-goto-top) "motion" "\

Purpose:
   Move to the top of the buffer.

Description:
   The function moves the editing position (point) to the top of the
   buffer.
" t nil)

(autoload (quote edstar-goto-bottom) "motion" "\

Purpose:
   Move to the bottom of the buffer.

Description:
   The function moves the editing position (point) to the bottom of the
   buffer.
" t nil)

(autoload (quote edstar-scroll-forward) "motion" "\

Purpose:
   Scroll towards the end of the buffer by about one window height.

Description:
   The function moves the editing position (point) towards the bottom of the
   buffer and positions it at the start of a line. The amount of movement is
   designed to display the next screenful of buffer text.

Notes:
   The number of lines moved is equal to the number of lines available for
   displaying text in the window, less the value \"next-screen-context-lines\",
   which provides some continuity between screens.
" t nil)

(autoload (quote edstar-scroll-reverse) "motion" "\

Purpose:
   Scroll towards the beginning of the buffer by about one window height.

Description:
   The function moves the editing position (point) towards the beginning of the
   buffer and positions it at the start of a line. The amount of movement is
   designed to display the previous screenful of buffer text.

Notes:
   The number of lines moved is equal to the number of lines available for
   displaying text in the window, less the value \"next-screen-context-lines\",
   which provides some continuity between screens.
" t nil)

(autoload (quote edstar-scroll) "motion" "\

Purpose:
   Scroll the buffer by about one window height.

Description:
   The function moves the editing position (point) towards the beginning or
   end of the buffer and positions it at the start of a line. The amount of
   movement is designed to display the adjacent screenful of buffer text. The
   direction of scrolling is determined by the current buffer direction
   setting (forward/reverse).

Notes:
   The number of lines moved is equal to the number of lines available for
   displaying text in the window, less the value \"next-screen-context-lines\",
   which provides some continuity between screens.
" t nil)

;;;***

;;; Generated autoloads from make_virtual.el

(autoload (quote edstar-make-virtual) "make_virtual" "\
Purpose:
   Turn a function into a \"virtual\" function.

Synopsis:
   (edstar-make-virtual fun args &optional doc interactive)

Description:
   This macro allows the creation of a virtual function (c.f. edstar-virtual)
   from an existing normal (non-virtual) function. The starting function may
   have been created using defun, or may be a built-in emacs function.

   Once a function has been converted into a virtual function, its
   implementation may be locally overridden using edstar-override (q.v.).

Arguments:
   fun
      The name of the function which is to be made virtual.
   args
      The argument list. This must match the argumant list of the original
      function.
   doc
      An optional documentation string. If supplied, it will override the
      function's original documentation string, if any.
   interactive
      An optional \"(interactive)\" declaration. If supplied, it will override
      the function's original declaration, if any. It may often be necessary
      to supply this declaration because it may not be possible to obtain the
      original one from the function supplied (e.g. if it is not a lisp
      function). If the function is interactively-callable, but no
      \"(interactive)\" declaration can be obtained, then a default one is
      used, although this may not always be appropriate.

Notes:
   - If using autoloading, you must ensure that the function being made
   virtual is loaded before using this macro. If this is not done, subsequent
   autoloading may cause the new definition to be over-written.
" nil (quote macro))

;;;***

;;; Generated autoloads from lookup_p.el

(autoload (quote edstar-lookup-place) "lookup_p" nil nil nil)

;;;***

;;; Generated autoloads from lookup_k.el

(autoload (quote edstar-lookup-keyword) "lookup_k" "\

Purpose:
   Look up a keyword in a table.

Description:
   Given a string containing the name (or an abbreviation) for a keyword
   (e.g. a token or helpkey) which is to be expanded, this function validates
   the name by looking it up in a keyword table. If found, it returns a symbol
   which contains the keyword information.

Arguments:
   name
      A string containing the keyword name, or an abbreviation (case
      insensitive). Abbreviations need not be unique; if necessary, a menu
      will be presented to the user to resolve any ambiguity.
   table
      An optional keyword table in which to find the name. If not given,
      edstar-token-table is used by default.

Returned Value:
   If the keyword name is valid, this function returns the symbol (interned
   in the table) which contains the keyword expansion information. Otherwise
   it returns nil.

Notes:
   If user interaction is necessary, then the user will have the option of
   aborting the keyword look-up. If this occurs, a value of nil will be
   returned, even though the keyword abbreviation was valid.
" nil nil)

;;;***

;;; Generated autoloads from learn.el
(defvar edstar-macro-key nil "Key to be used for current keyboard macro")

(autoload (quote learn) "learn" "\

Purpose:
   Define a keyboard macro (learn sequence).

Description:
   The function records a sequence of key presses and/or commands (a keyboard
   macro) and binds them to a key, so that they may be re-invoked by pressing
   that key. The sequence of events in defining a macro is as follows:

   1) Unless you specify a key when you invoke the \"learn\" function (you
   will not normally do so), you are first prompted to press the key which
   will be used to invoke the macro. Choose any key, or key sequence (using
   the GOLD- key for example), that you do not subsequently want to use for
   its original purpose.

   2) You should then press the keys and/or enter the commands which you
   want invoked. These will be executed as you enter them, so you can see
   their effect.

   3) When your definition is complete, press the original key (the one
   you pressed in step 1). This will terminate the learn sequence and cause
   this key to invoke the macro you have defined when you next press it.
" t nil)

(autoload (quote edstar-end-learn) "learn" "\

Purpose:
   End a keyboard macro definition (learn sequence).

Description:
   The function ends a keyboard macro definition begun by the \"learn\"
   command.

Notes:
   This function should not normally be invoked by a user except by pressing
   the macro kay at the end of defining a macro definition.

" t nil)

;;;***

;;; Generated autoloads from help.el
(defvar edstar-auto-help-flag t)

(autoload (quote auto-help) "help" nil t nil)

(autoload (quote edstar-help-on-this) "help" nil t nil)

(autoload (quote edstar-find-helpkey) "help" nil t nil)

;;;***

;;; Generated autoloads from handle_m.el

(autoload (quote edstar-handle-menu-motion) "handle_m" "\

Purpose:
   Control the motion of the cursor and window contents in a menu.

Description:
   This function is called by the functions invoked to perform cursor motion
   in a menu window created by edstar-select-from-menu. It moves the arrow
   cursor used to select from the menu (by re-drawing it). It also constrains
   the normal cursor motion to the first column and ensures that the menu
   window does not scroll beyond the beginning or end of the buffer which
   contains the menu options.

Returned Value:
   nil

Notes:
   The menu buffer should be current and displayed in a window before calling
   this function.

Global Variables:
   oldpos
      This is shared with edstar-select-from-window and records the previous
      position of the selection cursor.
" nil nil)

;;;***

;;; Generated autoloads from get_desc.el

(autoload (quote edstar-get-desc-string) "get_desc" nil t nil)

;;;***

;;; Generated autoloads from find_key.el

(autoload (quote edstar-find-keyword-string) "find_key" "\

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
" nil nil)

;;;***

;;; Generated autoloads from find.el

(autoload (quote edstar-find) "find" "\

Purpose:
   Find an existing file using the EDSTAR_PATH search path.

Description:
   The function searches for a file, looking in each directory specified by
   EDSTAR_PATH (as stored in the edstar-path list) in turn. If the file is
   found, its full name is returned as a string, otherwise nil is returned.

Arguments:
   file
      Name of the file to be found, as a string.

Notes:
   -  If \"\" or \".\" appear in the search path, they are interpreted as the
      current default directory, as given in edstar-default-directory.
   -  If \"..\" appears in the search path, it is interpreted as the parent
      directory of edstar-default-directory.
   -  Environment variable substitution and use of \"~\" in the file name are
      both supported.
   -  If the file name supplied contains a \"/\" character, then it is searched
      for explicitly and EDSTAR_PATH is not used (however the default directory
      edstar-default-directory is still used).
" nil nil)

;;;***

;;; Generated autoloads from file_pro.el

(autoload (quote edstar-file-prompt) "file_pro" "\

Purpose:
   Prompt for the name of a file to edit.

Description:
   The function prompts for the name of a file to edit, using the minibuffer,
   and returns the name supplied as a string. The file need not necessarily
   exist, but file name completion is provided, based on the set of files
   accessible in directories on the EDSTAR_PATH search path. Any files
   considered unsuitable for editing (e.g. with file types specified in the
   EDSTAR_IGNORE_FILES environment variable) are excluded when completing
   file names.

Arguments:
   prompt
      The string to be used to prompt the user.
   prefix (optional)
      An initial string to be entered into the minimuffer as a prefix for
      the file name.

Notes:
   A file name prefix, derived from the file name obtained, will be entered
   into the global variable edstar-file-prefix by this routine, for possible
   use as the \"prefix\" argument in subsequent invocations.
" nil nil)

(autoload (quote edstar-file-complete) "file_pro" "\

Purpose:
   File name completion predicate.

Description:
   The function is an instance of a file name \"completion predicate\" required
   by the completing-read function. It is used by edstar-file-prompt when
   completing file names based on files accessible in directories on the
   EDSTAR_PATH search path.

Arguments:
   str
      The prefix string to be completed.
   pred
      Predicate to test suitability of a completion.
   flag
      Flag indicating what this function should do.

Notes:
   -  This function acceses the external symbols filelist and laststr, which
      should normally be initialised to nil before invoking completing-read,
      otherwise the list of files used may be out of date.
   -  See the completing-read documentation for more details of what this
      function does.
" nil nil)

;;;***

;;; Generated autoloads from file_buff.el

(autoload (quote edstar-find-file-buffer) "file_buff" "\

Purpose:
   Find the buffer which is visiting a named file.

Description:
   The function returns the buffer which is visiting the file whose name
   is supplied. This function is similar to the emacs \"find-file-buffer\"
   function, except that the file is located relative to the EDSTAR default
   directory (as stored in the edstar-default-directory string) if the file
   name supplied is not absolute. This function also expands soft links to
   eliminate this possible source of file name confusion.

   The function returns nil if no buffer can be found.

Arguments:
   file
      A string giving the name of the file whose buffer is required. This
      should normally be an absolute file name, but a relative name will be
      expanded relative to the EDSTAR default directory by this function.
" nil nil)

;;;***

;;; Generated autoloads from exit.el

(autoload (quote exit) "exit" "\

Purpose:
   Exit cleanly from emacs.

Description:
   The function saves all modified buffers which are visiting files and then
   kills the current emacs session.
" t nil)

(autoload (quote quit) "exit" "\

Purpose:
  Abort the current emacs session.

Description:
  The function aborts the current emacs session without saving the contents
  of any modified buffers. If modified buffers exist (and they are visiting
  files), then confirmation is requested.
" t nil)

;;;***

;;; Generated autoloads from dump_tab.el

(autoload (quote edstar-dump-tables) "dump_tab" nil nil nil)

(autoload (quote edstar-dump-symbol) "dump_tab" nil nil nil)

;;;***

;;; Generated autoloads from do_env.el

(autoload (quote starfort-save-env) "do_env" nil t nil)

(autoload (quote starfort-rebuild-env) "do_env" nil t nil)

;;;***

;;; Generated autoloads from directio.el
(defvar edstar-forward t)
(make-variable-buffer-local 'edstar-forward)

(autoload (quote edstar-set-forward) "directio" nil t nil)

(autoload (quote edstar-set-reverse) "directio" nil t nil)

;;;***

;;; Generated autoloads from detach.el

(autoload (quote edstar-detach) "detach" "\

Purpose:
    Temporarily detach terminal from current editing session.

 Description:
    Any modified buffers are first flushed to their associated files and the
    current editing session is then suspended. Control is returned to the
    parent process (the one which activated emacs). Editing may be resumed by
    re-issuing the \"emacs\" command.

 Notes:
    Emacs cannot be suspended if it is running in X-windows mode. In this
    case, modified buffers are flushed and the current frame is iconified,
    but no suspension occurs.

 Arguments:
    None.
" t nil)

;;;***

;;; Generated autoloads from delete.el
(defvar edstar-char-was-deleted-forward t
"
Stores whether the last character deleted was deleted in the forward
direction or not. This determines whether it is inserted after or before
point when it is pasted back.
")

(autoload (quote edstar-delete-char) "delete" "\

Purpose:
   Delete the next character.

Description:
   The function deletes the character which follows the current editing
   position (point). The deleted text is stored in the \"c\" register and
   may subsequently be pasted back by using \"edstar-undelete-char\".
" t nil)

(autoload (quote edstar-delete-char-reverse) "delete" "\

Purpose:
   Delete the previous character.

Description:
   The function deletes the character which precedes the current editing
   position (point). The deleted text is stored in the \"c\" register and
   may subsequently be pasted back by using \"edstar-undelete-char\".

Notes:
   If the preceding character is a tab, it is first converted to an
   equivalent number of spaces to preserve alignment. The final resulting
   space is then deleted.
" t nil)

(autoload (quote edstar-undelete-char) "delete" "\

Purpose:
   Paste back the text deleted by \"edstar-delete-char\" (or
   \"edstar-delete-char-reverse\").

Description:
   The function pastes back the text last deleted by \"edstar-delete-char\"
   or \"edstar-delete-char-reverse\". The restored text is inserted immediately
   after the current editing position (point) if it was deleted in the
   forward direction, otherwise it is inserted immediately in front of the
   editing position.

Notes:
   The deleted text is recovered from the \"c\" register.
" t nil)

(autoload (quote edstar-delete-word) "delete" "\

Purpose:
   Delete text up to the next word.

Description:
   The function deletes the text between the current editing position (point)
   and the start of the following word (the criteria defining a word are the
   same as used by \"edstar-goto-word\", except that motion is always in the
   forward direction). The deleted text is stored in the \"w\" register and
   may subsequently be pasted back by using \"edstar-undelete-word\".
" t nil)

(autoload (quote edstar-undelete-word) "delete" "\

Purpose:
   Paste back text deleted by \"edstar-delete-word\".

Description:
   The function pastes back the text last deleted by \"edstar-delete-word\",
   inserting it immediately after the current editing position (point).

Notes:
   The deleted text is recovered from the \"w\" register.
" t nil)

(autoload (quote edstar-delete-line) "delete" "\

Purpose:
   Delete a line.

Description:
   The function deletes the text between the current editing position (point)
   and the start of the next line. The deleted text is stored in the \"l\"
   register and may subsequently be pasted back by using
   \"edstar-undelete-line\".
" t nil)

(autoload (quote edstar-delete-eol) "delete" "\

Purpose:
   Delete to the end of the line.

Description:
   The function deletes the text between the current editing position (point)
   and the end of the current line. The deleted text is stored in the \"l\"
   register and may subsequently be pasted back by using
   \"edstar-undelete-line\".

Notes:
   If the editing position is at the end of a line, text is deleted up to
   the end of the following line instead.
" t nil)

(autoload (quote edstar-undelete-line) "delete" "\

Purpose:
   Paste back text deleted by \"edstar-delete-line\" (or
   \"edstar-delete-eol\").

Description:
   The function pastes back the text last deleted by \"edstar-delete-line\"
   or \"edstar-delete-eol\", inserting it immediately after the curren
   editing position (point).

Notes:
   The deleted text is recovered from the \"l\" register.
" t nil)

;;;***

;;; Generated autoloads from cut.el

(autoload (quote edstar-cut) "cut" "\

Purpose:
   Cut out (delete) the selected text, storing it in the paste register.

Description:
   The function cuts the currently-selected text out of the current buffer
   (thus deleting it) and places it into the paste register (register \"p\"). It
   may later be pasted back elsewhere by using the \"edstar-paste\" function.

Notes:
   -  An error results if no text is selected.
   -  This function cancels the current selection.
" t nil)

(autoload (quote edstar-cut-append) "cut" "\

Purpose:
   Cut out (delete) the selected text, appending it to the paste register.

Description:
   The function cuts the currently-selected text out of the current buffer
   (thus deleting it) and appends it to the paste register (register \"p\"). It
   may later be pasted back elsewhere by using the \"edstar-paste\" function.

Notes:
   -  An error results if no text is selected.
   -  This function cancels the current selection.
" t nil)

(autoload (quote edstar-paste) "cut" "\

Purpose:
   Paste previously-cut text back into the current buffer.

Description:
   The function pastes back text which has previously been cut (using the
   function \"edstar-cut\") and stored in the paste register (register \"p\").
   The text is interted immediately before the current editing position
   (point).
" t nil)

(autoload (quote edstar-cut-replace) "cut" "\

Purpose:
   Cut the selected text, replacing it with the paste register contents.

Description:
   The function cuts the currently-selected text from the current buffer
   and replaces it with the contents of the paste register (register \"p\").
   The text which is cut out is stored in register \"s\".

Notes:
   -  An error results if no text is selected.
   -  This function cancels the current selection.
" t nil)

;;;***

;;; Generated autoloads from compile.el
(defvar edstar-error-index 0)

(autoload (quote com) "compile" nil t nil)

(autoload (quote forward-error) "compile" nil t nil)

(autoload (quote backward-error) "compile" nil t nil)

;;;***

;;; Generated autoloads from comment2.el

(autoload (quote starfort-line-break-context) "comment2" "\

Purpose:
   Determine how a line should be broken at the current position.

Description:
   The function returns a symbol indicating the context of the current editing
   position (point), indicating how the current line should be broken at that
   point so as to create a new line. It returns one of the following symbols:

     line-comment --> Point is in a full-line comment line.
     eol-comment  --> Point is inside an end-of-line (!) comment.
     code         --> Point is at the start or end of a line of code.
     continuation --> Point is inside a line of code, which must be continued.
     string       --> Point is inside a quoted string within a line of code.

Bugs:
   This function takes no account of code lines which are continued, so quoted
   strings should not be split across continuation lines.
" nil nil)

(autoload (quote starfort-break-line) "comment2" "\

Purpose:
   Break a line of Fortran at the current editing position.

Description:
   The function breaks a line of Fortran at the current editing position
   (point) to create a new line. If inside a comment line, a line of code or
   a quoted string (within code), then it is appropriately continued and
   indented on the new line.

Bugs:
   Full-line comment lines are not correctly indented when they are broken
   by this routine. More work needed here.
" t nil)

;;;***

;;; Generated autoloads from comment1.el

(autoload (quote edstar-insert-comment-line) "comment1" "\

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
" t nil)

;;;***

;;; Generated autoloads from choose_f.el

(autoload (quote edstar-choose-from-menu) "choose_f" "\

Purpose:
   Allow the user to choose an option from a menu.

Description:
   The functon displays a menu in a new window and allows the user to select
   an item from it. The user also has the option of aborting (i.e. not making
   a selection). The window is removed once the selection has been made.

Arguments:
   menu
      A list of strings, each of which will appear on its own line as a
      separate menu item.

Returned Value:
   An integer giving the index of the selected item in the menu list supplied
   (the first one is number 0).

Notes:
   If the user chooses to abort (i.e. not to make a selection), then nil is
   returned.

Global Variables:
   oldpos
      This is shared with the function edstar-handle-menu-motion and records
      the previous position of the selection cursor in the menu buffer.
" nil nil)

;;;***

;;; Generated autoloads from break.el

(autoload (quote starfort-line-break-context) "break" "\

Purpose:
   Determine how a line should be broken at the current position.

Description:
   The function returns a symbol indicating the context of the current editing
   position (point), indicating how the current line should be broken at that
   point so as to create a new line. It returns one of the following symbols:

     line-comment --> Point is in a full-line comment line.
     eol-comment  --> Point is inside an end-of-line (!) comment.
     code         --> Point is at the start or end of a line of code.
     continuation --> Point is inside a line of code, which must be continued.
     string       --> Point is inside a quoted string within a line of code.

Bugs:
   This function takes no account of code lines which are continued, so quoted
   strings should not be split across continuation lines.
" nil nil)

(autoload (quote starfort-backup-to-break) "break" "\

Purpose:
   Search backwards for a possible line break position.

Description:
   The function moves the current editing position (point) backwards on the
   current line until it locates a position which may be suitable for
   breaking the line. It returns the value of starfort-line-break-context
   at the resulting position.

Notes:
  -  The function searches backwards for a space which ends a word, or (if
  this would place it inside a quoted string) for the first character of a
  quoted word. If it cannot find a suitable position, point remains unchanged.
  -  This function should always leave point somewhere on the current line.
  -  This is a lower level routine and the position found must be further
  validated before use (e.g. by starfort-line-break-point) to protect against
  the possible creation of zero-length strings and to ensure that the required
  line length is not exceeded.
" nil nil)

(autoload (quote starfort-line-break-point) "break" "\

Purpose:
   Find the best line break point on the current line.

Description:
   The function moves point to the best possible position for breaking the
   current line so as to produce a new line. It attempts to break at white
   space if possible and to make the resulting broken line as long as possible
   while not extending past fill-column. It returns the value of the function
   starfort-line-break-context at the chosen position.
" nil nil)

(autoload (quote starfort-break-line) "break" "\

Purpose:
   Break a line of Fortran at the current editing position.

Description:
   The function breaks a line of Fortran at the current editing position
   (point) to create a new line. If inside a comment line, a line of code or
   a quoted string (within code), then it is appropriately continued and
   indented on the new line.

Arguments:
   context
      The value of the function starfort-line-break-context at the point
      where the line is to be broken. This indicates what type of break
      (and continuation) is required.

Bugs:
   Full-line comment lines are not correctly indented when they are broken
   by this routine. More work needed here.
" nil nil)

(autoload (quote starfort-do-auto-fill) "break" "\

Purpose:
   Perform auto-fill on Fortran lines.

Description:
   This function is intended for use by the auto-fill-hook in Starfort mode
   to perform automatic breaking of Fortran lines whenever a space character
   is entered beyond the fill-column (normally 71 in Starfort mode). It
   handles breaking of comment and code lines, including quoted strings,
   producing an appropriately indented continuation line.
" nil nil)

(autoload (quote starfort-newline) "break" "\

Purpose:
   Break the current line to create a new line.

Description:
   The function breaks a line of Fortran into two so as to create a new line.
   Comment lines, lines of code and quoted strings (within code) are
   appropriately continued and indented on the new line. If the editing
   position (point) is not initially beyond the buffer's fill column, the line
   is broken at the editing position. Otherwise, it is broken at the most
   suitable point which does not leave the first portion of the line extending
   beyond the fill column.

Notes:
   Placeholders will not be split by this function, even if the editing
   position lies within one.
" t nil)

(autoload (quote starfort-breakline) "break" nil nil nil)

;;;***

;;; Generated autoloads from align_tr.el

(autoload (quote edstar-align-trailing) "align_tr" nil t nil)

;;;***

;;; Generated autoloads from abs_file.el

(autoload (quote edstar-abs-file) "abs_file" "\

Purpose:
   Determine if a file name is absolute.

Description:
   The function returns non-nil if the string passed as an argument is an
   absolute file name, in the sense that it should not be searched for using
   any directory path. This will be the case if it contains a \"/\" character
   anywhere within it. (Note that relative file names containing \"/\" are
   considered \"absolute\" by this function.)
" nil nil)

;;;***
