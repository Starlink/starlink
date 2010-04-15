(new-routine "PSX_ASCTIME"
             "Convert a time structure to a character string"
             '("TSTRCT" "STRING" "STATUS"))

(new-routine "PSX_CALLOC"
             "Allocate space for several objects of specified type"
             '("NMEMB" "TYPE" "PNTR" "STATUS"))

(new-routine "PSX_CTIME"
             "Convert the calendar time to a character string"
             '("NTICKS" "STRING" "STATUS"))

(new-routine "PSX_CUSERID"
             "Get the username"
             '("USER" "STATUS"))

(new-routine "PSX_FREE"
             "Free virtual memory"
             '("PNTR" "STATUS"))

(new-routine "PSX_GETEGID"
             "Gets the effective group ID"
             '("GID" "STATUS"))

(new-routine "PSX_GETENV"
             "Translate an environment variable"
             '("NAME" "TRANS" "STATUS"))

(new-routine "PSX_GETEUID"
             "Gets the effective user ID"
             '("UID" "STATUS"))

(new-routine "PSX_GETGID"
             "Gets the real group ID"
             '("GID" "STATUS"))

(new-routine "PSX_GETPID"
             "Gets the process ID"
             '("PID" "STATUS"))

(new-routine "PSX_GETPPID"
             "Gets the process ID of the parent process"
             '("PID" "STATUS"))

(new-routine "PSX_GETUID"
             "Gets the real user ID"
             '("UID" "STATUS"))

(new-routine "PSX_ISATTY"
             "Determine if a file is a terminal"
             '("FILDSC" "ISTTY" "STATUS"))

(new-routine "PSX_LOCALTIME"
             "Convert the value returned by PSX_TIME to individual values"
             '("NTICKS" "SECS" "MINS" "HOURS" "DAY" "MONTH" "YEAR" "WDAY" "YDAY" "ISDST" "TSTRCT" "STATUS"))

(new-routine "PSX_MALLOC"
             "Allocate virtual memory"
             '("SIZE" "PNTR" "STATUS"))

(new-routine "PSX_RAND"
             "Generate a random number"
             '("INUM" "MAXNUM" "FNUM" "STATUS"))

(new-routine "PSX_REALLOC"
             "Change the size of an allocated region of virtual memory"
             '("SIZE" "PNTR" "STATUS"))

(new-routine "PSX_SRAND"
             "Set the seed for the random number generator"
             '("SEED" "STATUS"))

(new-routine "PSX_TIME"
             "Get the current calendar time"
             '("NTICKS" "STATUS"))

(new-routine "PSX_TTYNAME"
             "Get the name of the terminal"
             '("FILDSC" "TNAME" "STATUS"))

(new-routine "PSX_UNAME"
             "Gets information about the host computer system"
             '("SYSNAME" "NODENAME" "RELEASE" "VERSION" "MACHINE" "STATUS"))
