C*GRMSG -- issue message to user
C+
      SUBROUTINE GRMSG (MESS)
      CHARACTER*(*) MESS
C
C Display a message on standard output.
C
C Argument:
C  MESS (input): text of message to be printed (the string
C      may not be blank).
C--
C  8-Nov-1994 [TJP].
C  5-Aug-1999 [BKM}.
C   Modify for use in the Starlink environment.
C-----------------------------------------------------------------------
      INTEGER  STATUS, SAI__OK
      PARAMETER (SAI__OK = 0)
C
      STATUS = SAI__OK
      CALL MSG_OUT('PGPLOT_MSG', MESS, STATUS)

*   This routine is called by PGPLOT just prior to using graphical input so 
*   MSG_SYNC must be called to ensure that the message is delivered to the user
*   first.
      CALL MSG_SYNC(STATUS)

      END
