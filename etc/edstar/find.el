;;;###autoload
(defun edstar-find (file)
"
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
"

;;; Define local variables.
  (let ((dir edstar-path)
	(found nil)
	(fullfile)
	(sfile)
	(thisdir))

;;; Expand any environment variables in the file name.
    (setq sfile (substitute-in-file-name file))

;;; Test if the file name contains a "/".
    (if (edstar-abs-file sfile)

;;; If so, then EDSTAR_PATH will not be used. Expand to obtain an absolute
;;; file name, using edstar-default-directory if necessary.
	(progn
	    (setq fullfile (expand-file-name sfile edstar-default-directory))

;;; See if this file exists, returning its full name if it does.
	    (if (file-exists-p fullfile) fullfile nil))

;;; If EDSTAR_PATH is to be used, then loop to search in each directory in
;;; turn. Quit when the directory list is empty or the file is found.
      (while (and dir (not found))

;;; Extract the name of the next directory to search and expand it relative
;;; to the default directory.
	(setq thisdir (expand-file-name (car dir) edstar-default-directory))

;;; Get the full (absolute) file name, assuming it resides in this directory.
	(setq fullfile (expand-file-name sfile
					 (file-name-as-directory thisdir)))

;;; Note if the file exists.
	(setq found (file-exists-p fullfile))

;;; Increment to the next directory.
	(setq dir (cdr dir)))

;;; If the file exists, return its name.
      (if found fullfile nil))))
