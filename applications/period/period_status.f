
      SUBROUTINE PERIOD_STATUS(YPTR, MXCOL, MXSLOT, NPTSARRAY,
     :                         YERRORARRAY, INFILEARRAY, DETRENDARRAY,
     :                         LOGFILE, LOG, LOGUNIT)

C=============================================================================
C Routine to return information about the data currently stored by PERIOD.
C
C Written by Vikram Singh Dhillon @Sussex 4-June-1991.
C
C Converted to Double Precision (KPD), August 2001
C Power-raising modified to use INTEGER power (KPD), August 2001
C Modified to incorporate dynamic memory allocation for major
C  data/work array(s) and/or use of such arrays (KPD), October 2001
C=============================================================================

      IMPLICIT NONE

      INCLUDE 'CNF_PAR'

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

      INTEGER MXCOL, MXSLOT
      INTEGER YPTR(MXSLOT), NPTSARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C PERIOD_STATUS declarations.
C-----------------------------------------------------------------------------

      INTEGER FIRSTSLOT, LASTSLOT, I, SLOT, LOGUNIT
      INTEGER COUNTER, J, ENTRY, MAXLINES
      PARAMETER (MAXLINES=1000000)
      INTEGER DATAPTR
      INTEGER NDATA, YSLOT1
      LOGICAL YERRORARRAY(MXSLOT), DETRENDARRAY(MXSLOT), LOG
      CHARACTER*72 INFILEARRAY(MXSLOT), LOGFILE, STRING(50), DUMMY
      CHARACTER*1 , CONTINUE, OPTION


C-----------------------------------------------------------------------------
C Prompt for display mode.
C-----------------------------------------------------------------------------

      WRITE (*, *) ' '
 100  CONTINUE
      WRITE (*, '(X,A,$)') 'Display information on' //
     :                     '  [L]og file or [D]ata ? [D] : '
      READ (*, '(A)', ERR=100) OPTION
      CALL PERIOD_CASE(OPTION, .TRUE.)

C-----------------------------------------------------------------------------
C Return information on log file.
C-----------------------------------------------------------------------------

      IF ( OPTION.EQ.'L' ) THEN
         IF ( .NOT.LOG ) THEN
            CALL PERIOD_WRITEBELL()
            WRITE (*, *) '** ERROR: No log file has been opened.'
            GO TO 300
         END IF
         REWIND (LOGUNIT)
         WRITE (*, *) ' '
         COUNTER = 1
         ENTRY = 0
         DO 150 I = 1, MAXLINES
            READ (LOGUNIT, '(A)', END=200) DUMMY
            IF ( DUMMY(1:1).NE.'.' ) THEN
               STRING(COUNTER) = DUMMY
               COUNTER = COUNTER + 1
            ELSE
               ENTRY = ENTRY + 1
               WRITE (*, *) 'LOG FILE  = ', LOGFILE(1:60)
               WRITE (*, *) 'LOG ENTRY =', ENTRY
               DO 110 J = 1, COUNTER - 1
                  WRITE (*, *) STRING(J)
 110           CONTINUE
               WRITE (*, *) ' '
 120           CONTINUE
               WRITE (*, '(X,A,$)') '..... Hit return' //
     :                            ' to continue or any key to quit.....'
               READ (*, '(A)', ERR=120) CONTINUE
               WRITE (*, *) ' '
               IF ( CONTINUE.NE.' ' ) THEN
                  DO 125 J = 1, MAXLINES
                     READ (LOGUNIT, '(A)', END=300) DUMMY
 125              CONTINUE
               ELSE
                  COUNTER = 1
               END IF
            END IF
 150     CONTINUE
 200     CONTINUE
         IF ( I.EQ.1 ) THEN
            CALL PERIOD_WRITEBELL()
            WRITE (*, *) '** ERROR: Log file is empty.'
         END IF
C-----------------------------------------------------------------------------
C Return information on stored data.
C-----------------------------------------------------------------------------

      ELSE IF ( OPTION.EQ.'D' .OR. OPTION.EQ.' ' ) THEN
         WRITE (*, *) ' '
 250     CONTINUE
         WRITE (*, '(X,A,$)') 'Enter first and last slots for input' //
     :                        ' (0,0 to quit) : '
         READ (*, *, ERR=250) FIRSTSLOT, LASTSLOT

         IF ( FIRSTSLOT.NE.0 .AND. LASTSLOT.NE.0 ) THEN
            IF ( FIRSTSLOT.GT.MXSLOT .OR. LASTSLOT.GT.MXSLOT ) THEN
               CALL PERIOD_WRITEBELL()
               WRITE (*, *) '** ERROR: Maximum slot number = ', MXSLOT
               GO TO 300
            END IF

            DO 280 SLOT = FIRSTSLOT, LASTSLOT
               NDATA = NPTSARRAY(SLOT)

               IF ( NDATA.EQ.0 ) THEN
                  WRITE (*, *) ' '
                  WRITE (*, *) '** OK: No data present in slot number',
     :                         SLOT
               ELSE
                  YSLOT1 = YPTR(SLOT)

                  CALL PERIOD_ALLOC('_DOUBLE', NDATA, DATAPTR)

                  WRITE (*, *) ' '
                  WRITE (*, *) '** OK: Slot number = ', SLOT
                  WRITE (*, *) '** OK: ', INFILEARRAY(SLOT)(1:61)

                  CALL PERIOD_DATASTATUS(%VAL(CNF_PVAL(YSLOT1)), NDATA,
     :                                   MXCOL,
     :                                   YERRORARRAY(SLOT), MXSLOT,
     :                                   %VAL(CNF_PVAL(DATAPTR)))

                  CALL PERIOD_DEALL(DATAPTR)

                  IF ( DETRENDARRAY(SLOT) ) THEN
                     WRITE (*, *) '** OK: Detrended Y data = .TRUE.'
                  ELSE
                     WRITE (*, *) '** OK: Detrended Y data = .FALSE.'
                  END IF
                  WRITE (*, *) ' '
 265              CONTINUE
                  WRITE (*, '(X,A,$)') '..... Hit return' //
     :                            ' to continue or any key to quit.....'
                  READ (*, '(A)', ERR=265) CONTINUE
                  IF ( CONTINUE.NE.' ' ) GO TO 300

               END IF

 280        CONTINUE
         END IF
      ELSE
         GO TO 100
      END IF

 300  CONTINUE
      RETURN
      END
