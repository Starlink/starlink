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
C 8-Sep-1999
C  New version of Starlink ADAM environment. Applications are forbidden
C  to write to or read from the terminal directly.
C 
C-----------------------------------------------------------------------
      CALL GRWARN('GRGCOM: Illegal routine in Starlink evironment')
      GRGCOM = 0
      END
