;;; EDSTAR startup file.
;;; ===================

;;; Select the required optional emacs packages.
      (load "paren" nil t nil)   ; Parenthesis matching
      (cond (window-system       ; Mouse avoidance mode
	     (load "avoid" nil t nil)
	     (mouse-avoidance-mode 'animate)))

;;; Add the EDSTAR lisp directory to the load path.
      (setq load-path
	    (append (list nil (getenv "EDSTAR_DIR")) load-path))

;;; Re-compile any EDSTAR lisp code that needs it (but only if being run by
;;; the developer).
;;; --> (Should eventually remove this when the code is stable.)
;      (if (string-equal (user-login-name) "rfws")
;	  (byte-recompile-directory (getenv "EDSTAR_DIR") 0))

;;; Define the EDSTAR autoload functions.
      (load (concat (getenv "EDSTAR_DIR") "/loaddefs") nil t nil)

;;; Load the EDSTAR lisp packages that do not autoload.
      (load "check_sc" nil t nil)  ; Auto-scrolling
      (load "hi_place" nil t nil ) ; Highlight placeholders
      (load "mode_lin" nil t nil)  ; Put (column,line) on mode line
      (load "readonly" nil t nil ) ; Check for readonly directories
      (load "starc" nil t nil)     ; Derived mode for C editing

;;; Load the following lisp packages because depending on auto-loading for
;;; these causes annoying "autoloading xxx" messages to appear which
;;; interfere with prompting.
      (load "visit" nil t nil )
      (load "abs_file" nil t nil )
      (load "split_pa" nil t nil )
      (load "find" nil t nil )
      (load "possible" nil t nil )

;;; This package sets transient mark mode and may fail the first time it is
;;; used if it autoloads. This seems to be because the loading message modifies
;;; the message buffer and therefore deactivates the mark. Hence we load it
;;; here instead.
      (load "selection" nil t nil) ; Text selections

;;; Load definitions involving virtual functions.
      (load "newline" nil t nil)
      (load "breakline" nil t nil)
      (load "do_auto_fill" nil t nil)

;;; Establish the EDSTAR initialisation function.
      (load "init" nil t nil)

;;; Convert the EDSTAR_PATH search path into a list of directories, supplying
;;; a suitable default.
      (defvar edstar-path (edstar-split-path (or (getenv "EDSTAR_PATH") ".")))

;;; Convert the EDSTAR_READONLY search path into a list of read-only
;;; directories.
      (defvar edstar-readonly (edstar-split-path (getenv "EDSTAR_READONLY")))

;; Code to re-build placeholder and tokens tables from source.
;      (starfort-rebuild-env)
;      (starfort-save-env)

;;; Perform global emacs configuration.
      (setq comment-column 33)   ; Comment alignment column
      (setq require-final-newline t) ; Add final newline to buffers
      (transient-mark-mode 1)    ; Turn on transient mark mode

;;; Set the default major mode to text mode and set up the auto mode alist
;;; to recognise files that require new modes defined by EDSTAR.
      (setq default-major-mode 'text-mode)
      (setq text-mode-hook 'turn-on-auto-fill)
      (setq auto-mode-alist
	    (append
	     '(
	       ("\\.F$"       . starfort-mode)
	       ("\\.FOR$"     . starfort-mode)
	       ("\\.FORTRAN$" . starfort-mode)
	       ("\\.GEN$"     . starfort-mode)
	       ("\\.f$"       . starfort-mode)
	       ("\\.for$"     . starfort-mode)
	       ("\\.fortran$" . starfort-mode)
	       ("\\.gen$"     . starfort-mode)
	       ("\\.c$"       . starc-mode)
	       ("\\.h$"       . starc-mode)
	       ("\\.C$"       . starc-mode)
	       ("\\.H$"       . starc-mode)
	       ("\\.cc$"      . starc-mode)
	       ("\\.hh$"      . starc-mode)
	       ("\\.hxx$"     . starc-mode)
	       ("\\.cxx$"     . starc-mode)
	       ("\\.HXX$"     . starc-mode)
	       ("\\.CXX$"     . starc-mode)
	       ("\\.java$"    . starc-mode)
	       ) auto-mode-alist))

;;; Turn on the application keypad mode for the terminal. This works for
;;; VT-type terminals - don't know how to do it portably yet (maybe there
;;; is no way?).
      (send-string-to-terminal "\e[\e=")

;;; Define a keymap to handle "GOLD-key" sequences and define the kp-1 key
;;; to be the GOLD key.
      (defvar GOLD-map (make-sparse-keymap))
      (define-key global-map [kp-f1] GOLD-map)

;;; Define keys to perform EDSTAR functions (these are in addition to any
;;; keys already defined by emacs).
      (define-key GOLD-map   "="             'edstar-toggle-windows)
      (define-key GOLD-map   "a"             'other-window)
      (define-key GOLD-map   "b"             'switch-to-buffer)
      (define-key GOLD-map   "c"             'capitalize-word)
      (define-key GOLD-map   "f"             'goto-file)
      (define-key GOLD-map   "h"             'edstar-help-on-this)
      (define-key GOLD-map   "i"             'insert-file)
      (define-key GOLD-map   "l"             'learn)
      (define-key GOLD-map   "r"             'repeat-complex-command)
      (define-key GOLD-map   "s"             'edstar-save-buffers)
      (define-key GOLD-map   "u"             'upcase-word)
      (define-key GOLD-map   "w"             'save-buffer)
      (define-key GOLD-map   "z"             'exit)
      (define-key GOLD-map   [down]          'forward-paragraph)
      (define-key GOLD-map   [find]          'repeat-complex-command)
      (define-key GOLD-map   [kp-0]          'open-line)
;      (define-key GOLD-map   [kp-f2]         'help-with-tutorial)
      (define-key GOLD-map   [kp-f2]         help-map)
      (define-key GOLD-map   [kp-1]          'edstar-toggle-case)
      (define-key GOLD-map   [kp-2]          'edstar-delete-eol)
      (define-key GOLD-map   [kp-4]          'edstar-goto-bottom)
      (define-key GOLD-map   [kp-5]          'edstar-goto-top)
      (define-key GOLD-map   [kp-6]          'edstar-paste)
      (define-key GOLD-map   [kp-7]          'execute-extended-command)
      (define-key GOLD-map   [kp-8]          'fill-region)
      (define-key GOLD-map   [kp-9]          'edstar-cut-replace)
      (define-key GOLD-map   [kp-decimal]    'edstar-unset-selection)
      (define-key GOLD-map   [kp-enter]      'query-replace)
      (define-key GOLD-map   [kp-f1]         'undefined)
      (define-key GOLD-map   [kp-f3]         'edstar-search)
      (define-key GOLD-map   [kp-f4]         'edstar-undelete-line)
      (define-key GOLD-map   [kp-enter]      'edstar-undelete-char)
      (define-key GOLD-map   [kp-subtract]   'edstar-undelete-word)
      (define-key GOLD-map   [left]          'backward-word)
      (define-key GOLD-map   [next]          'edstar-goto-bottom)
      (define-key GOLD-map   [prior]         'edstar-goto-top)
      (define-key GOLD-map   [up]            'backward-paragraph)
      (define-key global-map "\C-^"          'edstar-insert-comment-line)
      (define-key global-map "\C-b"          'backward-error)
      (define-key global-map "\C-e"          'expand-indicated)
      (define-key global-map "\C-f"          'forward-error)
      (define-key global-map "\C-k"          'erase-placeholder)
      (define-key global-map "\C-m"          'newline-and-indent)
      (define-key global-map "\C-n"          'next-placeholder)
      (define-key global-map "\C-p"          'previous-placeholder)
      (define-key global-map [127]           'edstar-delete-char-reverse) ; DEL
      (define-key global-map [down]          'move-down)
      (define-key global-map [f11]           'edstar-save-buffers)
      (define-key global-map [f12]           'edstar-list-buffers)
      (define-key global-map [find]          'search-forward)
      (define-key global-map [help]          'help-for-help)
      (define-key global-map [insert]        'edstar-paste)
      (define-key global-map [kp-0]          'edstar-next-line)
      (define-key global-map [kp-1]          'edstar-goto-word)
      (define-key global-map [kp-2]          'edstar-goto-eol)
      (define-key global-map [kp-3]          'forward-char)
      (define-key global-map [kp-4]          'edstar-set-forward)
      (define-key global-map [kp-5]          'edstar-set-reverse)
      (define-key global-map [kp-6]          'edstar-cut)
      (define-key global-map [kp-7]          'edstar-goto-page)
      (define-key global-map [kp-8]          'edstar-scroll)
      (define-key global-map [kp-9]          'edstar-cut-append)
      (define-key global-map [kp-decimal]    'edstar-set-selection)
      (define-key global-map [kp-f2]         'help-for-help)
      (define-key global-map [kp-f3]         'edstar-search-again)
      (define-key global-map [kp-f4]         'edstar-delete-line)
      (define-key global-map [kp-enter]      'edstar-delete-char)
      (define-key global-map [kp-subtract]   'edstar-delete-word)
      (define-key global-map [left]          'move-left)
      (define-key global-map [menu]          'execute-extended-command)
      (define-key global-map [next]          'edstar-scroll-forward)
      (define-key global-map [prior]         'edstar-scroll-reverse)
      (define-key global-map [right]         'move-right)
      (define-key global-map [select]        'edstar-select-buffer)
      (define-key global-map [up]            'move-up)
      (define-key global-map "\t"            'indent-according-to-mode)

;;; Read the "attach key" definition from the EDSTAR_ATTACH_KEY environment
;;; variable. If defined, make the appropriate key assignment to run the
;;; detach procedure.
      (let ((key (getenv "EDSTAR_ATTACH_KEY")))
	(if key (define-key global-map (read key) 'edstar-detach)))
