C*GRGCOM -- read with prompt from user's terminal (Sun/Convex-UNIX)
C+
      INTEGER FUNCTION GRGCOM(STRING, PROMPT, L)
      CHARACTER*(*) STRING, PROMPT
      INTEGER L
C
C Issue prompt and read a line from the user's terminal; in VMS,
C this is equivalent to LIB$GET_COMMAND.
C
C Arguments:
C  STRING : (output) receives the string read from the terminal.
C  PROMPT : (input) prompt string.
C  L      : (output) length of STRING.
C
C Returns:
C  GRGCOM : 1 if successful, 0 if an error occurs (e.g., end of file).
C--
C 9-Feb-1988
C 10-Feb-1990 revised to always read from stdin (unit 5), but issue a
C             prompt only when device is a terminal.
C-----------------------------------------------------------------------
      INTEGER IER
C
      GRGCOM = 0
      L = 0
      IER = 0
      WRITE (*, '(1X,A,$)', IOSTAT=IER) PROMPT
      IF (IER.EQ.0) READ (*, '(A)', IOSTAT=IER) STRING
      IF (IER.EQ.0) GRGCOM = 1
      L = LEN(STRING)
   10 IF (STRING(L:L).NE.' ') GOTO 20
          L = L-1
          GOTO 10
   20 CONTINUE
      END
