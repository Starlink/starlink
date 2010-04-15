

      SUBROUTINE PERIOD_WINDOW(YPTR, MXCOL, MXSLOT, NPTSARRAY,
     :                         YERRORARRAY, INFILEARRAY, DETRENDARRAY)

C==============================================================================
C Sets all data points to unity so that the window function can be generated.
C
C Written by Vikram Singh Dhillon @IAC 31-January-1992.
C
C Converted to Double Precision (KPD), August 2001
C Modified to incorporate dynamic memory allocation for major
C  data/work array(s) and/or use of such arrays (KPD), October 2001
C==============================================================================

      IMPLICIT NONE

      INCLUDE 'CNF_PAR'

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

      INTEGER MXCOL, MXSLOT
      INTEGER YPTR(MXSLOT), NPTSARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C PERIOD_WINDOW declarations.
C-----------------------------------------------------------------------------

      DOUBLE PRECISION WINDOW, EWINDOW
      INTEGER NDATA, FIRSTSLOT, LASTSLOT, SLOT, FIRSTOUT, IFAIL
      INTEGER COUNTER, SLOTOUT
      INTEGER YSLOT1, YSLOT2
      LOGICAL YERRORARRAY(MXSLOT), DETRENDARRAY(MXSLOT)
      CHARACTER*72 INFILEARRAY(MXSLOT)
      DATA WINDOW, EWINDOW/1.0D0, 0.0D0/

C-----------------------------------------------------------------------------
C Select input and output data slots.
C-----------------------------------------------------------------------------

      WRITE (*, *) ' '
      CALL PERIOD_SELECT(FIRSTSLOT, LASTSLOT, FIRSTOUT, MXSLOT, IFAIL)
      IF ( IFAIL.EQ.1 ) GO TO 200
      WRITE (*, *) ' '

      COUNTER = 0

      DO 100 SLOT = FIRSTSLOT, LASTSLOT
         NDATA = NPTSARRAY(SLOT)

         IF ( NDATA.EQ.0 ) THEN
            CALL PERIOD_WRITEBELL()
            WRITE (*, *) '** ERROR: Slot empty =', SLOT
            GO TO 200
         END IF

         SLOTOUT = FIRSTOUT + COUNTER
         YSLOT1 = YPTR(SLOT)

         IF ( SLOT.NE.SLOTOUT) THEN
            IF ( NPTSARRAY(SLOTOUT).NE.0 )
     :         CALL PERIOD_DEALL(YPTR(SLOTOUT))
            CALL PERIOD_ALLOC('_DOUBLE', NDATA*MXCOL, YPTR(SLOTOUT))
         END IF

         YSLOT2 = YPTR(SLOTOUT)
         COUNTER = COUNTER + 1

         CALL PERIOD_SETUPWINDOW(%VAL(CNF_PVAL(YSLOT1)), NDATA, MXCOL,
     :               WINDOW, EWINDOW, %VAL(CNF_PVAL(YSLOT2)))

         YERRORARRAY(SLOTOUT) = .FALSE.
         DETRENDARRAY(SLOTOUT) = .FALSE.
         NPTSARRAY(SLOTOUT) = NPTSARRAY(SLOT)
         INFILEARRAY(SLOTOUT) = 'Windowed ' // INFILEARRAY(SLOT)
         WRITE (*, *) '** OK: Filled slot = ', SLOTOUT
 100  CONTINUE

 200  CONTINUE
      RETURN
      END
