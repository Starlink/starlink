;;; Define global variable to note how many windows to select.
;;;###autoload (defvar edstar-select-one-window nil)

;;;###autoload
(defun edstar-list-buffers ()
"
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
"
  (interactive)

;;; Note whether there is initialy only one window displayed.
  (setq edstar-select-one-window (one-window-p))

;;; Create and select the list of buffers.
  (buffer-menu t))

;;;###autoload
(defun edstar-select-buffer ()
"
Purpose:
   Select a buffer in Buffer Menu mode.

Description:
   The function selects the buffer identified by the cursor when in Buffer
   Menu mode. It is equivalent to typing \"1\" or \"2\" when in this mode,
   except that it decides whether to display one or 2 windows depending on
   how many windows were displayed prior to the last \"edstar-list-buffers\"
   command.
"
   (interactive)

;;; Select the buffer, using one or two windows as required.
   (if edstar-select-one-window
       (Buffer-menu-1-window)
     (Buffer-menu-2-window)))
