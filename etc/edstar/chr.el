(new-routine "CHR_APPND"
             "Copy one string into another (ignoring trailing blanks)"
             '("STR1" "STR2" "LEN2"))

(new-routine "CHR_CLEAN"
             "Remove all non-printable ASCII characters from a string"
             '("STRING"))

(new-routine "CHR_COPY"
             "Copy one string to another, checking for truncation"
             '("INSTR" "FLAG" "OUTSTR" "LSTAT"))

(new-routine "CHR_CTOC"
             "Write a character value into a string (in concise format)"
             '("CVALUE" "STRING" "NCHAR"))

(new-routine "CHR_CTOD"
             "Read a double precision number from a character string"
             '("CVALUE" "DVALUE" "STATUS"))

(new-routine "CHR_CTOI"
             "Read an integer number from a character string"
             '("CVALUE" "IVALUE" "STATUS"))

(new-routine "CHR_CTOL"
             "Read a logical value from a character string"
             '("CVALUE" "LVALUE" "STATUS"))

(new-routine "CHR_CTOR"
             "Read a real number from a character string"
             '("CVALUE" "RVALUE" "STATUS"))

(new-routine "CHR_DCWRD"
             "Return all the words in a character string"
             '("STRING" "MXWRD" "NWRD" "START" "STOP" "WORDS" "LSTAT"))

(new-routine "CHR_DELIM"
             "Locate indices to substring with given delimiter character"
             '("STRING" "DELIM" "INDEX1" "INDEX2"))

(new-routine "CHR_DTOC"
             "Encode a double precision number as a character string"
             '("DVALUE" "CVALUE" "NCHAR"))

(new-routine "CHR_EQUAL"
             "Determine whether two strings are equal"
             '("STR1" "STR2"))

(new-routine "CHR_FANDL"
             "Find the indices of the first and last non-blank characters"
             '("STRING" "INDEX1" "INDEX2"))

(new-routine "CHR_FILL"
             "Fill a string with a given character"
             '("CHAR" "STRING"))

(new-routine "CHR_FIWE"
             "Find next end of word"
             '("STRING" "INDEX" "STATUS"))

(new-routine "CHR_FIWS"
             "Find next start of word"
             '("STRING" "INDEX" "STATUS"))

(new-routine "CHR_HTOI"
             "Read an integer number from a hexadecimal string"
             '("STRING" "IVALUE" "STATUS"))

(new-routine "CHR_INDEX"
             "Find the index of a substring in a string"
             '("STRING" "SUBSTR"))

(new-routine "CHR_INSET"
             "Determine whether a string is a member of a set"
             '("SET" "STRING"))

(new-routine "CHR_ISALF"
             "Determine whether a character is alphabetic"
             '("CHAR"))

(new-routine "CHR_ISALM"
             "Determine whether a character is alphanumeric"
             '("CHAR"))

(new-routine "CHR_ISDIG"
             "Determine whether a character is a digit"
             '("CHAR"))

(new-routine "CHR_ISNAM"
             "Determine whether a string is a valid name"
             '("STRING"))

(new-routine "CHR_ITOC"
             "Encode an integer number as a character string"
             '("IVALUE" "CVALUE" "NCHAR"))

(new-routine "CHR_LCASE"
             "Convert a string to lower case"
             '("STRING"))

(new-routine "CHR_LDBLK"
             "Remove leading blanks from a string"
             '("STRING"))

(new-routine "CHR_LEN"
             "Find the used length of a string"
             '("STRING"))

(new-routine "CHR_LOWER"
             "Give lower case equivalent of a character"
             '("CHAR"))

(new-routine "CHR_LTOC"
             "Encode a logical value as a character string"
             '("LVALUE" "CVALUE" "NCHAR"))

(new-routine "CHR_MOVE"
             "Move one string into another (ignoring trailing blanks)"
             '("STR1" "STR2"))

(new-routine "CHR_OTOI"
             "Read an integer number from an octal string"
             '("STRING" "IVALUE" "STATUS"))

(new-routine "CHR_PUTC"
             "Copy one string into another"
             '("STR1" "STR2" "LEN2"))

(new-routine "CHR_PUTD"
             "Put a double precision number into a string at a given position"
             '("DVALUE" "STRING" "LENGTH"))

(new-routine "CHR_PUTI"
             "Put an integer value into a string at a given position"
             '("IVALUE" "STRING" "LENGTH"))

(new-routine "CHR_PUTL"
             "Put a logical value into a string at a given position"
             '("LVALUE" "STRING" "LENGTH"))

(new-routine "CHR_PUTR"
             "Put a real number into a string at a given position"
             '("RVALUE" "STRING" "LENGTH"))

(new-routine "CHR_RMBLK"
             "Remove all blanks from a string in situ"
             '("STRING"))

(new-routine "CHR_RTOAN"
             "Write a real number into a string as hr/deg:min:sec"
             '("RVALUE" "UNITS" "STRING" "LENGTH"))

(new-routine "CHR_RTOC"
             "Encode a real number as a character string"
             '("RVALUE" "CVALUE" "NCHAR"))

(new-routine "CHR_SIMLR"
             "Determine whether two strings are equal apart from case"
             '("STR1" "STR2"))

(new-routine "CHR_SIZE"
             "Find the declared size of a string"
             '("STRING"))

(new-routine "CHR_SWAP"
             "Swap two single-character variables"
             '("V1" "V2"))

(new-routine "CHR_TERM"
             "Terminate string by padding out with blanks"
             '("LENGTH" "STRING"))

(new-routine "CHR_TRUNC"
             "Truncate string rightwards from a given character"
             '("DELIM" "STRING"))

(new-routine "CHR_UCASE"
             "Convert a string to upper case"
             '("STRING"))

(new-routine "CHR_UPPER"
             "Give upper case equivalent of a character"
             '("CHAR"))
