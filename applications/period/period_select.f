

      SUBROUTINE PERIOD_SELECT(FIRSTSLOT, LASTSLOT, FIRSTOUT, MXSLOT,
     :                         IFAIL)

C=============================================================================
C Simple routine to select input and output slots.
C
C Written by Vikram Singh Dhillon @Sussex 29-March-1992.
C=============================================================================

      IMPLICIT NONE

      INTEGER FIRSTSLOT, LASTSLOT, MXSLOT, IFAIL
      INTEGER FIRSTOUT, LASTOUT

C-----------------------------------------------------------------------------
C Select input and output data
C-----------------------------------------------------------------------------

 100  CONTINUE
      WRITE (*, '(X,A,$)') 'Enter first and last slots for input ' //
     :                     ' (0,0 to quit) : '
      READ (*, *, ERR=100) FIRSTSLOT, LASTSLOT
      IF ( FIRSTSLOT.EQ.0 .OR. LASTSLOT.EQ.0 ) GO TO 300
      IF ( FIRSTSLOT.GT.MXSLOT .OR. LASTSLOT.GT.MXSLOT ) THEN
         CALL PERIOD_WRITEBELL()
         WRITE (*, *) '** ERROR: Maximum slot number =', MXSLOT
         GO TO 300
      END IF
 200  CONTINUE
      WRITE (*, '(X,A,$)') 'Enter first and last slots for output' //
     :                     ' (0,0 to quit) : '
      READ (*, *, ERR=200) FIRSTOUT, LASTOUT
      IF ( FIRSTOUT.EQ.0 .OR. LASTOUT.EQ.0 ) THEN
      ELSE IF ( FIRSTOUT.GT.MXSLOT .OR. LASTOUT.GT.MXSLOT ) THEN
         CALL PERIOD_WRITEBELL()
         WRITE (*, *) '** ERROR: Maximum slot number =', MXSLOT
      ELSE IF ( (LASTOUT-FIRSTOUT).NE.(LASTSLOT-FIRSTSLOT) ) THEN
         CALL PERIOD_WRITEBELL()
         WRITE (*, *) '** ERROR: Unequal number of input and' //
     :                ' output slots.'
      ELSE
         IFAIL = 0
         RETURN
      END IF

 300  CONTINUE
      IFAIL = 1

      RETURN
      END
