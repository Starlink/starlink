(new-routine "FIO_ACTIV"
             "Initialise FIO library for ADAM application"
             '("STATUS"))

(new-routine "FIO_ASSOC"
             "Create/open a sequential file associated with a parameter"
             '("PNAME" "ACMODE" "FORM" "RECSZ" "FD" "STATUS"))

(new-routine "FIO_CANCL"
             "Close a file and cancel the parameter"
             '("PNAME" "STATUS"))

(new-routine "FIO_CLOSE"
             "Close a sequential file"
             '("FD" "STATUS"))

(new-routine "FIO_DEACT"
             "Deactivate FIO"
             '("STATUS"))

(new-routine "FIO_ERASE"
             "Delete a file from the file-base"
             '("FILE" "STATUS"))

(new-routine "FIO_FNAME"
             "Get the full file name of a file"
             '("FD" "FNAME" "STATUS"))

(new-routine "FIO_GUNIT"
             "Get a unit number"
             '("UNIT" "STATUS"))

(new-routine "FIO_OPEN"
             "Create/open a sequential file"
             '("FILE" "ACMODE" "FORM" "RECSZ" "FD" "STATUS"))

(new-routine "FIO_PUNIT"
             "Release a unit number"
             '("UNIT" "STATUS"))

(new-routine "FIO_READ"
             "Read sequential record"
             '("FD" "BUF" "NCHAR" "STATUS"))

(new-routine "FIO_READF"
             "Fast read sequential record"
             '("FD" "BUF" "STATUS"))

(new-routine "FIO_REP"
             "Report error from FORTRAN I/O statements"
             '("UNIT" "FNAME" "IOSTAT" "MESS" "STATUS"))

(new-routine "FIO_RWIND"
             "Rewind a sequential file"
             '("FD" "STATUS"))

(new-routine "FIO_SERR"
             "Set error status"
             '("IOSTAT" "STATUS"))

(new-routine "FIO_START"
             "Set up units numbers and open standard I/O streams"
             '("STATUS"))

(new-routine "FIO_STOP"
             "Close down FIO"
             '("STATUS"))

(new-routine "FIO_TEST"
             "Test if an FIO status value belongs to a certain class of errors"
             '("ERRCLS" "STATUS"))

(new-routine "FIO_UNIT"
             "Get a unit number given a file descriptor"
             '("FD" "UNIT" "STATUS"))

(new-routine "FIO_WRITE"
             "Write a sequential record"
             '("FD" "BUF" "STATUS"))

(new-routine "RIO_ASSOC"
             "Create/open a direct access file associated with a parameter"
             '("PNAME" "ACMODE" "FORM" "RECSZ" "FD" "STATUS"))

(new-routine "RIO_CANCL"
             "Close a file and cancel the parameter"
             '("PNAME" "STATUS"))

(new-routine "RIO_CLOSE"
             "Close a direct access file"
             '("FD" "STATUS"))

(new-routine "RIO_ERASE"
             "Delete a file from the file-base"
             '("FILE" "STATUS"))

(new-routine "RIO_OPEN"
             "Open a direct access file"
             '("FILE" "ACMODE" "FORM" "RECSZ" "FD" "STATUS"))

(new-routine "RIO_READ"
             "Read record from direct access file"
             '("FD" "RECNO" "NCHAR" "BUF" "STATUS"))

(new-routine "RIO_WRITE"
             "Write a record to a direct access file"
             '("FD" "RECNO" "NCHAR" "BUF" "STATUS"))
