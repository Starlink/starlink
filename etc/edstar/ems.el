(let (new-ems-routine)

;;; Define a local function to create a token for an EMS routine with
;;; associated argumant placeholders and a helpkey.
  (fset 'new-ems-routine
        (lambda (name desc args &optional help)
          (if (not help) (setq help name))         ; use routine name to find
                                                   ; help by default
          (new-routine name desc args
                       (list (cons 'helpkey name)) ; routine name is helpkey so
                                                   ; put this into plist
                       "starfort")
          (new-helpkey name (list "ssn4" help)     ; this is where the external
                                                   ; help can be found
                       nil "starfort")))

  (new-ems-routine "EMS_ANNUL"
                   "Annul the contents of the current error context"
                   '("status"))

  (new-ems-routine "EMS_BEGIN"
                   "Begin a new error reporting environment"
                   '("status"))

  (new-ems-routine "EMS_ELOAD"
                   "Return error messages from the current error context"
                   '("param" "parlen" "opstr" "oplen" "status"))

  (new-ems-routine "EMS_END"
                   "End the current error reporting environment"
                   '("status"))

  (new-ems-routine "EMS_FIOER"
                   "Assign a Fortran I/O error message to a token"
                   '("token" "iostat"))

  (new-ems-routine "EMS_FMTC"
                   "Assign a CHARACTER value to a message token (formatted)"
                   '("token" "format" "cvalue")
                   "EMS_FMTx")

  (new-ems-routine "EMS_FMTD"
                   "Assign a DOUBLE PRECISION value to a message token (formatted)"
                   '("token" "format" "dvalue")
                   "EMS_FMTx")

  (new-ems-routine "EMS_FMTI"
                   "Assign an INTEGER value to a message token (formatted)"
                   '("token" "format" "ivalue")
                   "EMS_FMTx")

  (new-ems-routine "EMS_FMTL"
                   "Assign a LOGICAL value to a message token (formatted)"
                   '("token" "format" "lvalue")
                   "EMS_FMTx")

  (new-ems-routine "EMS_FMTR"
                   "Assign a REAL value to a message token (formatted)"
                   '("token" "format" "rvalue")
                   "EMS_FMTx")

  (new-ems-routine "EMS_LEVEL"
                   "Inquire the current error context level"
                   '("level"))

  (new-ems-routine "EMS_MARK"
                   "Start a new error context"
                   ( ))

  (new-ems-routine "EMS_MLOAD"
                   "Expand and return a message"
                   '("param" "text" "opstr" "oplen" "status"))

  (new-ems-routine "EMS_RENEW"
                   "Renew any annulled message tokens in the current context"
                   ( ))

  (new-ems-routine "EMS_REP"
                   "Report an error message"
                   '("param" "text" "status"))

  (new-ems-routine "EMS_RLSE"
                   "Release (end) an error context"
                   ( ))

  (new-ems-routine "EMS_SETC"
                   "Assign a CHARACTER value to a message token (concise)"
                   '("token" "cvalue")
                   "EMS_SETx")

  (new-ems-routine "EMS_SETD"
                   "Assign a DOUBLE PRECISION value to a message token (concise)"
                   '("token" "dvalue")
                   "EMS_SETx")

  (new-ems-routine "EMS_SETI"
                   "Assign an INTEGER value to a message token (concise)"
                   '("token" "ivalue")
                   "EMS_SETx")

  (new-ems-routine "EMS_SETL"
                   "Assign a LOGICAL value to a message token (concise)"
                   '("token" "lvalue")
                   "EMS_SETx")

  (new-ems-routine "EMS_SETR"
                   "Assign a REAL value to a message token (concise)"
                   '("token" "rvalue")
                   "EMS_SETx")

  (new-ems-routine "EMS_STAT"
                   "Inquire the last reported error status"
                   '("status"))

  (new-ems-routine "EMS_SYSER"
                   "Assign an operating system error message to a token"
                   '("token" "systat")))
