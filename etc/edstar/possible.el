;;;###autoload
(defun edstar-possible-files (prefix)
"
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
"

;;; Define local variables.
   (let ( (files ())
	  (alist ())
	  (last "")
	  (badtypes)
	  (dir edstar-path))

;;; Set up a pattern for matching file names, supplying a default prefix
;;; which matches all files (not beginning with ".") if necessary.
     (if (not prefix) (setq prefix "^[^\\.].*")
       (setq prefix (concat "^" prefix ".*" )))

;;; Loop to search for files in each of the EDSTAR_PATH directories.
     (while dir

;;; Obtain a list of file names from the directory. Append the list to those
;;; names already obtained.
       (setq files (append files (directory-files (car dir) nil prefix)))

;;; Increment to search the next directory.
       (setq dir (cdr dir)))

;;; Sort the complete list of file names into reverse alphabetical order.
     (setq files (sort files '(lambda (str1 str2) (string< str2 str1))))

;;; Obtain a regular expression to match the file type extensions of any files
;;; which are to be excluded. Supply a default if necessary.
     (setq badtypes (getenv "EDSTAR_IGNORE_FILES"))
     (if (not badtypes) (setq badtypes "o\\|a\\|tar\\|Z"))

;;; Loop to purge the file name list of duplicates and unwanted names.
     (while files

;;; Only consider file names which are not the same as the previous one
;;; processed.
       (if (not (string= last (car files)))

;;; Remember the current file name for testing the next one.
	   (progn
	     (setq last (car files))

;;; Test if the file type extension matches any of the excluded types of file.
;;; If OK, cons the name on to the front of the final alist, using t as its
;;; association.
	     (if (not (string-match
		       (concat ".*\\.\\(" badtypes "\\)$") (car files)))
		 (setq alist (cons (cons (car files) t) alist)))))

;;; Increment to consider the next file name.
       (setq files (cdr files)))

;;; Return the final alist.
     alist))
