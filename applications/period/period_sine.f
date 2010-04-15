
      SUBROUTINE PERIOD_SINE(YPTR, MXCOL, MXSLOT, NPTSARRAY,
     :                       DETRENDARRAY, YERRORARRAY, INFILEARRAY)

C===========================================================================
C Adds, subtracts, multiplies or divides sine curves from data.
C
C Written by Vikram Singh Dhillon @Sussex 10-February-1992.
C
C Converted to Double Precision (KPD), August 2001
C===========================================================================

      IMPLICIT NONE

      INCLUDE 'CNF_PAR'

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

      INTEGER MXCOL, MXSLOT
      INTEGER YPTR(MXSLOT), NPTSARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C PERIOD_SINE declarations.
C-----------------------------------------------------------------------------

      DOUBLE PRECISION PERIOD, AMPLITUDE, ZEROPT, GAMMA
      INTEGER SLOT, FIRSTSLOT, LASTSLOT, NDATA
      INTEGER FIRSTOUT, IFAIL, COUNTER, SLOTOUT
      INTEGER YSLOT1, YSLOT2
      LOGICAL DETRENDARRAY(MXSLOT), YERRORARRAY(MXSLOT)
      CHARACTER*1 OPTION
      CHARACTER*72 INFILEARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C Select input and output data slots.
C-----------------------------------------------------------------------------

      WRITE (*, *) ' '
      CALL PERIOD_SELECT(FIRSTSLOT, LASTSLOT, FIRSTOUT, MXSLOT, IFAIL)
      IF ( IFAIL.EQ.1 ) GO TO 400

C-----------------------------------------------------------------------------
C Prompt for SINE option.
C-----------------------------------------------------------------------------

      WRITE (*, *) ' '
 100  CONTINUE
      WRITE (*, '(X,A,$)') 'Enter period, semi-amplitude, zero point' //
     :                     ' and gamma : '
      READ (*, *, ERR=100) PERIOD, AMPLITUDE, ZEROPT, GAMMA
      IF ( PERIOD.LE.0.0D0 ) THEN
         CALL PERIOD_WRITEBELL()
         WRITE (*, *) '** ERROR: Invalid value for PERIOD.'
         GO TO 100
      END IF
 200  CONTINUE
      WRITE (*, '(X,A,$)') '[A]dd, [S]ubtract, [M]ultiply' //
     :                     ' or [D]ivide sine curve ? [S] : '
      READ (*, '(A)', ERR=200) OPTION
      CALL PERIOD_CASE(OPTION, .TRUE.)

      WRITE (*, *) ' '
      COUNTER = 0

      DO 300 SLOT = FIRSTSLOT, LASTSLOT
         NDATA = NPTSARRAY(SLOT)

         IF ( NDATA.EQ.0 ) THEN
            CALL PERIOD_WRITEBELL()
            WRITE (*, *) '** ERROR: Slot empty =', SLOT
            GO TO 400
         END IF

         SLOTOUT = FIRSTOUT + COUNTER
         COUNTER = COUNTER + 1
         YSLOT1 = YPTR(SLOT)

         IF ( SLOT.NE.SLOTOUT ) THEN
            IF ( NPTSARRAY(SLOTOUT).NE.0 )
     :         CALL PERIOD_DEALL(YPTR(SLOTOUT))
            CALL PERIOD_ALLOC('_DOUBLE', NDATA*MXCOL, YPTR(SLOTOUT))
         END IF

         YSLOT2 = YPTR(SLOTOUT)

C-----------------------------------------------------------------------------
C Add, subtract, multiply or divide sine curve.
C-----------------------------------------------------------------------------

         CALL PERIOD_MODSINECURVE(%VAL(CNF_PVAL(YSLOT1)),
     :               NDATA, MXCOL,
     :               PERIOD, AMPLITUDE, ZEROPT,
     :               GAMMA, OPTION, %VAL(CNF_PVAL(YSLOT2)))

         DETRENDARRAY(SLOTOUT) = .FALSE.
         YERRORARRAY(SLOTOUT) = YERRORARRAY(SLOT)
         NPTSARRAY(SLOTOUT) = NPTSARRAY(SLOT)

         IF ( OPTION.EQ.'A' ) THEN
            INFILEARRAY(SLOTOUT) = 'Sine Added ' // INFILEARRAY(SLOT)
         ELSE IF ( OPTION.EQ.'M' ) THEN
            INFILEARRAY(SLOTOUT) = 'Sine Multiplied ' //
     :                             INFILEARRAY(SLOT)
         ELSE IF ( OPTION.EQ.'D' ) THEN
            INFILEARRAY(SLOTOUT) = 'Sine Divided ' // INFILEARRAY(SLOT)
         ELSE IF ( OPTION.EQ.'S' .OR. OPTION.EQ.' ' ) THEN
            INFILEARRAY(SLOTOUT) = 'Sine Subtracted ' //
     :                             INFILEARRAY(SLOT)
         END IF
         WRITE (*, *) '** OK: Filled slot = ', SLOTOUT

 300  CONTINUE

 400  CONTINUE
      RETURN
      END
