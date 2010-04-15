(let (new-msg-routine)

;;; Define a local function to create a token for a MSG routine with
;;; associated argumant placeholders and a helpkey.
  (fset 'new-msg-routine
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

  (new-msg-routine "MSG_BLANK"
                   "Output a blank line"
                   '("status"))

  (new-msg-routine "MSG_FMTC"
                   "Assign a CHARACTER value to a message token (formatted)"
                   '("token" "format" "cvalue")
                   "MSG_FMTx")

  (new-msg-routine "MSG_FMTD"
                   "Assign a DOUBLE PRECISION value to a message token (formatted)"
                   '("token" "format" "dvalue")
                   "MSG_FMTx")

  (new-msg-routine "MSG_FMTI"
                   "Assign an INTEGER value to a message token (formatted)"
                   '("token" "format" "ivalue")
                   "MSG_FMTx")

  (new-msg-routine "MSG_FMTL"
                   "Assign a LOGICAL value to a message token (formatted)"
                   '("token" "format" "lvalue")
                   "MSG_FMTx")

  (new-msg-routine "MSG_FMTR"
                   "Assign a REAL value to a message token (formatted)"
                   '("token" "format" "rvalue")
                   "MSG_FMTx")

  (new-msg-routine "MSG_IFGET"
                   "Get the filter level for conditional message output from the ADAM parameter system"
                   '("pname" "status"))

  (new-msg-routine "MSG_IFLEV"
                   "Return the current filter level for conditional message output"
                   '("filter"))

  (new-msg-routine "MSG_IFSET"
                   "Set the filter level for conditional message output"
                   '("filter" "status"))

  (new-msg-routine "MSG_LOAD"
                   "Expand and return a message"
                   '("param" "text" "opstr" "oplen" "status"))

  (new-msg-routine "MSG_OUT"
                   "Output a message"
                   '("param" "text" "status"))

  (new-msg-routine "MSG_OUTIF"
                   "Conditionally deliver the text of a message to the user"
                   '("prior" "param" "text" "status"))

  (new-msg-routine "MSG_RENEW"
                   "Renew any annulled message tokens in the current context"
                   ( ))

  (new-msg-routine "MSG_SETC"
                   "Assign a CHARACTER value to a message token (concise)"
                   '("token" "cvalue")
                   "MSG_SETx")

  (new-msg-routine "MSG_SETD"
                   "Assign a DOUBLE PRECISION value to a message token (concise)"
                   '("token" "dvalue")
                   "MSG_SETx")

  (new-msg-routine "MSG_SETI"
                   "Assign an INTEGER value to a message token (concise)"
                   '("token" "ivalue")
                   "MSG_SETx")

  (new-msg-routine "MSG_SETL"
                   "Assign a LOGICAL value to a message token (concise)"
                   '("token" "lvalue")
                   "MSG_SETx")

  (new-msg-routine "MSG_SETR"
                   "Assign a REAL value to a message token (concise)"
                   '("token" "rvalue")
                   "MSG_SETx")

  (new-msg-routine "MSG_SYNC"
                   "Synchronise message output via the user interface"
                   '("status")))
