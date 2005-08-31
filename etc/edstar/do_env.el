;;; Save the starfort environment to a file.
;;;###autoload
(defun starfort-save-env (&optional file)
  (interactive)
  (setq file (or file "env.el"))
  (edstar-dump-tables '( starfort-mode-placeholder-table starfort-mode-token-table starfort-mode-helpkey-table )
		      file 'env))

;;; Rebuild the starfort environment.
;;;###autoload
(defun starfort-rebuild-env ()
  (interactive)
  (makunbound 'starfort-mode-placeholder-table)
  (makunbound 'starfort-mode-token-table)
  (makunbound 'starfort-mode-helpkey-table)
  (let ((edstar-placeholder-table 'starfort-mode-placeholder-table)
        (edstar-token-table 'starfort-mode-token-table)
        (edstar-helpkey-table 'starfort-mode-helpkey-table))
    (load "load_pac")
    (load "adam_con")
    (load "def_pl")
    (load "def_pl_a")
    (load "define_a")))
