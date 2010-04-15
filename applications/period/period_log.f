
      SUBROUTINE PERIOD_LOG(LOGFILE, LOG, LOGUNIT, EXIST)

C=============================================================================
C Routine to open and close a log file for PERIOD.
C
C Written by Vikram Singh Dhillon @LPO 27-January-1992.
C
C Added EXIST to parameter list - GJP March 1997
C
C=============================================================================

      IMPLICIT NONE

C-----------------------------------------------------------------------------
C PERIOD_LOG declarations.
C-----------------------------------------------------------------------------

      INTEGER LOGUNIT, I, NUMENT
      LOGICAL LOG, EXIST
      CHARACTER*72 LOGFILE, JUNK
      CHARACTER*1 REPLY
      DATA NUMENT/1000000/

      IF ( LOG ) THEN

         IF ( EXIST ) THEN

            CALL PERIOD_WRITEBELL()
            WRITE (*, *) '** ERROR: Log file has already been opened.'
            GO TO 100

         ELSE

            WRITE (*, *) ' '
 20         CONTINUE
            WRITE (*, '(X,A,$)') 'Enter name of log file : '
            READ (*, '(A)', ERR=20) LOGFILE
 40         CONTINUE
            WRITE (*, '(X,A,$)') '[N]ew or [O]ld file ? [N] : '
            READ (*, '(A)', ERR=40) REPLY
            CALL PERIOD_CASE(REPLY, .TRUE.)
            IF ( REPLY.EQ.'O' ) THEN
               OPEN (UNIT=LOGUNIT, FILE=LOGFILE, STATUS='OLD', ERR=20)
               EXIST = .TRUE.
               DO 50 I = 1, NUMENT
                  READ (LOGUNIT, '(A)', END=60) JUNK
 50            CONTINUE
 60            CONTINUE
               WRITE (*, *) ' '
               WRITE (*, *) '** OK: Opened old log file = ',
     :                      LOGFILE(1:43)
               WRITE (*, *) '** OK: Number of lines skipped over = ',
     :                      I - 1
               WRITE (*, *) ' '

            ELSE

               OPEN (UNIT=LOGUNIT, FILE=LOGFILE, STATUS='NEW', ERR=20)
               EXIST = .TRUE.
               WRITE (*, *) ' '
               WRITE (*, *) '** OK: Opened new log file = ',
     :                      LOGFILE(1:43)
               WRITE (*, *) ' '

            END IF

         END IF

      ELSE IF ( EXIST ) THEN

         CLOSE (UNIT=LOGUNIT)
         WRITE (*, *) ' '
         WRITE (*, *) '** OK: Log file has been closed = ',
     :                LOGFILE(1:39)
         EXIST = .FALSE.

      ELSE

         CALL PERIOD_WRITEBELL()
         WRITE (*, *) '** ERROR: No log file has been opened.'
         GO TO 100

      END IF

 100  CONTINUE
      RETURN
      END
