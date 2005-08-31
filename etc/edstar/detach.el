;;;###autoload
(defun edstar-detach ()
"
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
"
   (interactive)

;;; Save modified buffers without prompting.
   (save-some-buffers t)

;;; If working in character cell mode, simply suspend emacs. When execution
;;; resumes, edstar-init will execute and re-initialise.
   (if (not window-system)
       (suspend-emacs)

;;; If working in windows mode, iconify the frame. When the parent process
;;; requests resumption, edstar-resume-windows will execute to re-initialise
;;; and de-iconify the frame. Note that this may also happen asynchonously
;;; (i.e. without edstar-detach having first been called).
     (iconify-frame)))
