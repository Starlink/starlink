
      SUBROUTINE PERIOD_QUIT

C=======================================================================
C Pathetic little routine to quit from a program.
C
C Written by Vikram Singh Dhillon @Sussex 1-July-1992.
C
C Unused variable BELL removed - GJP June 1995
C
C=======================================================================

      IMPLICIT NONE

      CHARACTER*1 REPLY

      WRITE (*, *) ' '
 100  CONTINUE
      WRITE (*, '(X,A,$)') 'Are you sure that you want to quit ? [N] : '
      READ (*, '(A)', ERR=100) REPLY
      CALL PERIOD_CASE(REPLY, .TRUE.)
      IF ( REPLY.EQ.'Y' ) THEN
         WRITE (*, *) ' '
         WRITE (*,*) '** OK: PERIOD closing down. Goodbye.'
         STOP ' '
      END IF

      RETURN
      END
