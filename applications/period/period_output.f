
      SUBROUTINE PERIOD_OUTPUT(YPTR, MXCOL, MXSLOT, NPTSARRAY,
     :                         YERRORARRAY)

C===========================================================================
C Output data to a disk file.
C
C Written by Vikram Singh Dhillon @LPO 27-January-1992.
C
C GJP March 1997
C
C Removed the upper case conversion on the file name.
C
C Converted to Double Precision (KPD), August 2001
C Modified to incorporate dynamic memory allocation for major
C  data/work array(s) and/or use of such arrays (KPD), October 2001
C===========================================================================

      IMPLICIT NONE

      INCLUDE 'CNF_PAR'

      INCLUDE 'SAE_PAR'

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

      INTEGER MXCOL, MXSLOT
      INTEGER YPTR(MXSLOT), NPTSARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C PERIOD_OUTPUT declarations.
C-----------------------------------------------------------------------------

      INTEGER FIRSTSLOT, LASTSLOT, SLOT, IUNIT
      INTEGER NDATA, YSLOT1, STATUS
      LOGICAL YERRORARRAY(MXSLOT)
      CHARACTER*32 OUTFILE

C-----------------------------------------------------------------------------
C Initialise variables
C-----------------------------------------------------------------------------

      STATUS = SAI__OK
      CALL FIO_GUNIT( IUNIT, STATUS )

C-----------------------------------------------------------------------------
C Select slots to output.
C-----------------------------------------------------------------------------

      WRITE (*, *) ' '
 100  CONTINUE
      WRITE (*, '(X,A,$)') 'Enter first and last slots for output' //
     :                     ' (0,0 to quit) : '
      READ (*, *, ERR=100) FIRSTSLOT, LASTSLOT
      IF ( FIRSTSLOT.NE.0 .AND. LASTSLOT.NE.0 ) THEN
         IF ( FIRSTSLOT.GT.MXSLOT .OR. LASTSLOT.GT.MXSLOT ) THEN
            CALL PERIOD_WRITEBELL()
            WRITE (*, *) '** ERROR: Maximum slot number =', MXSLOT
            GO TO 200
         END IF

         DO 150 SLOT = FIRSTSLOT, LASTSLOT
            NDATA = NPTSARRAY(SLOT)

            IF ( NDATA.EQ.0 ) THEN
               CALL PERIOD_WRITEBELL()
               WRITE (*, *) '** ERROR: Slot empty =', SLOT
               GO TO 200
            END IF

 120        CONTINUE
            WRITE (*, '(X,A,$)') 'Enter output filename (q to quit) : '
            READ (*, '(A)', ERR=120) OUTFILE
            IF ((OUTFILE.EQ.'Q').OR.(OUTFILE.EQ.'q')) GO TO 200
            OPEN (UNIT=IUNIT, FILE=OUTFILE, STATUS='NEW', ERR=120)

            YSLOT1 = YPTR(SLOT)

            CALL PERIOD_PERFORMOUTPUT(%VAL(CNF_PVAL(YSLOT1)),
     :                                NDATA, MXCOL,
     :                                YERRORARRAY(SLOT), IUNIT)

            CLOSE (UNIT=IUNIT)
            WRITE (*, *) ' '
            WRITE (*, *) '** OK: Saved slot = ', SLOT
            WRITE (*, *) ' '

 150     CONTINUE

      END IF

 200  CONTINUE
      CALL FIO_PUNIT( IUNIT, STATUS )
      RETURN
      END
