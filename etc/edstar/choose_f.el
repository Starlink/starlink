;;;###autoload
(defun edstar-choose-from-menu (menu)
"
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
"

;;; Define local variables.
  (let (buffer oldpos oldmap keymap abort)

;;; Save the current buffer and window status.
    (save-excursion
      (save-window-excursion

;;; Create a new menu buffer (if necessary) and make it current.
	(set-buffer (setq buffer (get-buffer-create "*Menu Options*")))

;;; Ensure the buffer is writeable and erase its contents. Ensure that long
;;; lines are truncated (not wrapped).
	(setq buffer-read-only nil)
	(erase-buffer)
	(setq truncate-lines t)

;;; Set the mode line to contain instructions on how to select from the menu.
	(setq mode-line-format
	      (concat "   Use vertical motion keys to select... "
		      "Press RET when ready (C-g to abort)."))

;;; Loop to write each line of the menu into the buffer.
	(while menu
	  (insert "   " (car menu) "\n")
	  (setq menu (cdr menu)))

;;; Remove the final newline (to prevent a blank line at the end of the menu)
;;; and reposition at the start.
	(delete-char -1)
	(goto-char (point-min))

;;; Write an arrow cursor to point at the first menu item.
	(delete-char 3)
	(insert "-->")

;;; Re-position at the start and save the current position (used by the cursor
;;; movement handling function).
	(goto-char (point-min))
	(setq oldpos (point))

;;; Save the current global keymap and make a new (empty) one to be used in
;;; the menu buffer.
	(setq oldmap (current-global-map))
	(setq keymap (make-keymap))

;;; Define vertical motion keys in the new keymap:

;;; Move up a line...
	(define-key keymap [up] '(lambda () (interactive) ; up arrow
				   (forward-line -1)
				   (edstar-handle-menu-motion)))

;;; Move down a line...
	(define-key keymap [down] '(lambda () (interactive) ; down arrow
				     (forward-line)
				     (edstar-handle-menu-motion)))
	(define-key keymap [kp-0] (lookup-key keymap [down])) ; keypad zero

;;; Move up a screen...
	(define-key keymap [prior] ; previous screen
	  '(lambda () (interactive)
	     (if (not (pos-visible-in-window-p (point-min)))
		 (scroll-down)
	       (goto-char (point-min))
	       (recenter 0))
	     (edstar-handle-menu-motion)))

;;; Move down a screen...
	(define-key keymap [next] ; next screen
	  '(lambda () (interactive)
	     (if (not (pos-visible-in-window-p (point-max)))
		 (scroll-up)
	       (goto-char (point-max))
	       (recenter -1))
	     (edstar-handle-menu-motion)))

;;; Move to top of menu...
	(define-key keymap [kp-f1 kp-5] ; keypad GOLD-5
	  '(lambda () (interactive)
	     (goto-char (point-min))
	     (recenter 0)
	     (edstar-handle-menu-motion)))

;;; Move to bottom of menu...
	(define-key keymap [kp-f1 kp-4] ; keypad GOLD-4
	  '(lambda () (interactive)
	     (goto-char (point-max))
	     (recenter -1)
	     (edstar-handle-menu-motion)))

;;; Define RET to exit the menu with a selection. Ctrl-e is a synonym.
	(define-key keymap "\C-m" 'exit-recursive-edit)
	(define-key keymap "\C-e" 'exit-recursive-edit)

;;; Define C-g to abort (indicated by setting abort).
	(define-key keymap "\C-g" '(lambda () (interactive)
				     (setq abort t)
				     (exit-recursive-edit)))

;;; Activate the new global keymap and switch off any local keymap (note the
;;; global keymap be restored before any more normal editing can be done).
	(use-global-map keymap)
	(use-local-map nil)

;;; Make the buffer read-only and display it in a new window,
	(setq buffer-read-only t)
	(pop-to-buffer buffer)

;;; Recursively edit the menu buffer.
	(recursive-edit)

;;; When done, restore the global keymap (very important).
	(use-global-map oldmap)

;;; If abort was set, return nil.
	(if abort
	    nil

;;; Otherwise, return the line number which the user selected in the menu
;;; buffer.
	  (count-lines (point-min) (point)))))))

