C*GRQUIT -- report a fatal error and abort execution
C+
      SUBROUTINE GRQUIT (TEXT)
      CHARACTER*(*) TEXT
C
C Report a fatal error (via GRWARN) and exit program.
C This routine should be called in the event of an unrecoverable 
C PGPLOT error.
C
C Argument:
C  TEXT (input): text of message to be sent to GRWARN.
C--
C 12-Nov-1994
C-----------------------------------------------------------------------
C
      CALL GRWARN(TEXT)
      CALL GRWARN('Fatal error in PGPLOT library: program terminating.')
      STOP 
      END
