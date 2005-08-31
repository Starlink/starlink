;;;###autoload
(defun edstar-find-file-buffer (file)
"
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
"

;;; Define local variables.
  (let ((buffer)
        (bufferfile)
        (bufferlist)
        (found nil)
        (fullfile))

;;; Fully expand the file name given relative to the EDSTAR default directory.
;;; Also expand soft links.
    (setq fullfile (file-truename
                    (expand-file-name file edstar-default-directory)))

;;; Obtain the buffer list and loop to examine each buffer which appears in it.
    (setq bufferlist (buffer-list))
    (while (and bufferlist (not found))
      (setq buffer (car bufferlist))

;;; Check if the buffer is visiting a file.
      (if (setq bufferfile (buffer-file-name buffer))

;;; If so, expand soft links in the buffer's file name and compare it with the
;;; file name we want.
          (setq found (string= fullfile
                               (file-truename
                                (expand-file-name bufferfile
                                                  edstar-default-directory)))))

;;; Consider the next buffer.
      (setq bufferlist (cdr bufferlist)))

;;; Return the buffer, if found. Otherwise return nil.
    (if found buffer nil)))
