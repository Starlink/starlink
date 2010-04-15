(let (new-err-routine)

;;; Define a local function to create a token for an ERR routine with
;;; associated argumant placeholders and a helpkey.
  (fset 'new-err-routine
        (lambda (name desc args &optional help)
          (if (not help) (setq help name))         ; use routine name to find
                                                   ; help by default
          (new-routine name desc args
                       (list (cons 'helpkey name)) ; routine name is helpkey so
                                                   ; put this into plist
                       "starfort")
          (new-helpkey name (list "sun104" help)   ; this is where the external
                                                   ; help can be found
                       nil "starfort")))


  (new-err-routine "ERR_ANNUL"
                   "Annul the contents of the current error context"
                   '("status"))

  (new-err-routine "ERR_BEGIN"
                   "Create a new error reporting environment"
                   '("status"))

  (new-err-routine "ERR_END"
                   "End the current error reporting environment"
                   '("status"))

  (new-err-routine "ERR_FIOER"
                   "Assign a Fortran I/O error message to a token"
                   '("token" "iostat"))

  (new-err-routine "ERR_FLUSH"
                   "Flush the current error context"
                   '("status"))

  (new-err-routine "ERR_LEVEL"
                   "Inquire the current error context level"
                   '("level"))

  (new-err-routine "ERR_LOAD"
                   "Return error messages from the current error context"
                   '("param" "parlen" "opstr" "oplen" "status"))

  (new-err-routine "ERR_MARK"
                   "Mark (start) a new error context"
                   ( ))

  (new-err-routine "ERR_REP"
                   "Report an error message"
                   '("param" "text" "status"))

  (new-err-routine "ERR_RLSE"
                   "Release (end) the current error context"
                   ( ))

  (new-err-routine "ERR_STAT"
                   "Inquire the last reported error status"
                   '("status"))

  (new-err-routine "ERR_SYSER"
                   "Assign an operating system error message to a token"
                   '("token" "systat")))
