
      SUBROUTINE PERIOD_DETREND(YPTR, MXCOL, MXSLOT, NPTSARRAY,
     :                          YERRORARRAY, DETRENDARRAY, INFILEARRAY)

C===========================================================================
C Detrends data by either subtracting a polynomial fit or
C subtracting the mean and dividing by the standard deviation.
C
C Written by Vikram Singh Dhillon @LPO 25-January-1992.
C
C GJP March 1997
C
C Modified so that tests for CHISQ = -1,-2,-3 are safer.
C
C Converted to Double Precision (KPD), August 2001
C Power-raising modified to use INTEGER power (KPD), August 2001
C Modified to incorporate dynamic memory allocation for major
C  data/work array(s) and/or use of such arrays (KPD), October 2001
C===========================================================================

      IMPLICIT NONE

      INCLUDE "mnmxvl.h"

      INCLUDE 'CNF_PAR'

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

      INTEGER MXCOL, MXSLOT
      INTEGER YPTR(MXSLOT), NPTSARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C PERIOD_DETREND declarations.
C-----------------------------------------------------------------------------

      INTEGER    MAXPOLY
      PARAMETER (MAXPOLY=10)
      DOUBLE PRECISION AVE, ADEV, SDEV, VAR
      INTEGER    NDATA, NPOLY, SLOT, FIRSTSLOT, LASTSLOT
      INTEGER    FIRSTOUT, IFAIL, SLOTOUT, COUNTER, NORM
      INTEGER    DATAPTR, XPTR
      INTEGER    YSLOT1, YSLOT2
      LOGICAL    YERRORARRAY(MXSLOT), DETRENDARRAY(MXSLOT)
      DOUBLE PRECISION PFT(MAXPOLY), CHISQ
      DOUBLE PRECISION XM(MAXPOLY, 2*MAXPOLY+3)
      CHARACTER*1  OPTION, REPLY
      CHARACTER*72 INFILEARRAY(MXSLOT)


C-----------------------------------------------------------------------------
C Select input and output data slots.
C-----------------------------------------------------------------------------

      WRITE (*, *) ' '
      CALL PERIOD_SELECT(FIRSTSLOT, LASTSLOT, FIRSTOUT, MXSLOT, IFAIL)
      IF ( IFAIL.EQ.1 ) GO TO 400
      WRITE (*, *) ' '
 100  CONTINUE
      WRITE (*, '(X,A,$)') 'Detrend using the [M]ean or a' //
     :                     ' [P]olynomial fit ? [M] : '
      READ (*, '(A)', ERR=100) OPTION
      CALL PERIOD_CASE(OPTION, .TRUE.)
      IF ( OPTION.NE.'P' .AND. OPTION.NE.'M' .AND. OPTION.NE.' ' )
     :     GO TO 400
      IF ( OPTION.EQ.'P' ) THEN
 150     CONTINUE
         WRITE (*, '(X,A,$)') 'Enter order of polynomial : '
         READ (*, *, ERR=150) NPOLY
         IF ( NPOLY.GT.MAXPOLY ) THEN
            CALL PERIOD_WRITEBELL()
            WRITE (*, *) '** ERROR: Maximum polynomial order = ',
     :                   MAXPOLY
            GO TO 400
         ELSE IF ( NPOLY.LE.0 ) THEN
            CALL PERIOD_WRITEBELL()
            WRITE (*, *) '** ERROR: Minimum polynomial order = 1'
            GO TO 400
         END IF
      END IF

      COUNTER = 0

      DO 300 SLOT = FIRSTSLOT, LASTSLOT
         NDATA = NPTSARRAY(SLOT)

         IF ( NDATA.EQ.0 ) THEN
            CALL PERIOD_WRITEBELL()
            WRITE (*, *) '** ERROR: Slot empty =', SLOT
            GO TO 400
         END IF

         IF ( DETRENDARRAY(SLOT) ) THEN
            CALL PERIOD_WRITEBELL()
            WRITE (*, *) '** WARNING: Slot already detrended = ', SLOT
 160        CONTINUE
            WRITE (*, '(X,A,$)') '** WARNING: Are you sure you want' //
     :                           ' to continue ? [N] : '
            READ (*, '(A)', ERR=160) REPLY
            CALL PERIOD_CASE(REPLY, .TRUE.)
            IF ( REPLY.NE.'Y' ) GO TO 400
         END IF

         SLOTOUT = FIRSTOUT + COUNTER
         COUNTER = COUNTER + 1
         YSLOT1 = YPTR(SLOT)

         IF ( OPTION.EQ.'P' ) THEN
            IF ( NPOLY.GE.NDATA ) THEN
               CALL PERIOD_WRITEBELL()
               WRITE (*, *)
     :                   '** ERROR: Number of polynomial terms greater '
     :                   // 'than or equal'
               WRITE (*, *) '** ERROR: to the number of data points. '
     :                      // 'Fit aborted.'
               GO TO 400
            END IF

            CALL PERIOD_ALLOC('_DOUBLE', NDATA*3, XPTR)

            CALL PERIOD_SETX(%VAL(CNF_PVAL(YSLOT1)), NDATA, MXCOL,
     :                       YERRORARRAY(SLOT), %VAL(CNF_PVAL(XPTR)))

            NORM = 1
            CALL PERIOD_LSQUAR(%VAL(CNF_PVAL(XPTR)), NDATA, NPOLY,
     :                         PFT, CHISQ,
     :                         XM, NORM)

            CALL PERIOD_DEALL(XPTR)

C            IF ( CHISQ.EQ.-1.0D0 ) THEN
            IF ( DABS(CHISQ+1).LT.DPMN30 ) THEN
               CALL PERIOD_WRITEBELL()
               WRITE (*, *) '** ERROR: Singular matrix in' //
     :                      ' PERIOD_LSQUAR.'
               GO TO 400
C            ELSE IF ( CHISQ.EQ.-2.0D0 ) THEN
            ELSE IF ( DABS(CHISQ+2).LT.DPMN30 ) THEN
               CALL PERIOD_WRITEBELL()
               WRITE (*, *) '** ERROR: Overflow or divide check' //
     :                      ' occurred in PERIOD_LSQUAR.'
               GO TO 400
C            ELSE IF ( CHISQ.EQ.-3.0D0 ) THEN
            ELSE IF ( DABS(CHISQ+3).LT.DPMN30 ) THEN
               CALL PERIOD_WRITEBELL()
               WRITE (*, *) '** ERROR: Invalid parameters input' //
     :                      ' to PERIOD_LSQUAR.'
               GO TO 400
            END IF

            IF ( SLOT.NE.SLOTOUT ) THEN
               IF ( NPTSARRAY(SLOTOUT).NE.0 )
     :            CALL PERIOD_DEALL(YPTR(SLOTOUT))
               CALL PERIOD_ALLOC('_DOUBLE', NDATA*MXCOL, YPTR(SLOTOUT))
            END IF

            YSLOT2 = YPTR(SLOTOUT)

            CALL PERIOD_PUTYPOLY(%VAL(CNF_PVAL(YSLOT1)),
     :                           NDATA, MXCOL, PFT,
     :                           MAXPOLY, NPOLY, %VAL(CNF_PVAL(YSLOT2)))

            DETRENDARRAY(SLOTOUT) = .TRUE.
            YERRORARRAY(SLOTOUT) = YERRORARRAY(SLOT)
            INFILEARRAY(SLOTOUT) = 'Detrended ' // INFILEARRAY(SLOT)
            NPTSARRAY(SLOTOUT) = NDATA

         ELSE

            CALL PERIOD_ALLOC('_DOUBLE', NDATA, DATAPTR)

            CALL PERIOD_SETDATA(%VAL(CNF_PVAL(YSLOT1)), NDATA, MXCOL, 2,
     :                          %VAL(CNF_PVAL(DATAPTR)))

            CALL PERIOD_MOMENT(%VAL(CNF_PVAL(DATAPTR)),
     :                         NDATA, AVE, ADEV, SDEV,
     :                         VAR)

            CALL PERIOD_DEALL(DATAPTR)

            WRITE (*, *) ' '
            WRITE (*, *) '** OK: Y data mean = ', AVE
            WRITE (*, *) '** OK: Y data standard deviation = ', SDEV
            IF ( SDEV.EQ.0.0D0 ) THEN
               CALL PERIOD_WRITEBELL()
               WRITE (*, *) '** ERROR: Standard deviation is zero.'
               GO TO 400
            END IF

            IF ( SLOT.NE.SLOTOUT ) THEN
               IF ( NPTSARRAY(SLOTOUT).NE.0 )
     :            CALL PERIOD_DEALL(YPTR(SLOTOUT))
               CALL PERIOD_ALLOC('_DOUBLE', NDATA*MXCOL, YPTR(SLOTOUT))
            END IF

            YSLOT2 = YPTR(SLOTOUT)

            CALL PERIOD_PUTYMEAN(%VAL(CNF_PVAL(YSLOT1)),
     :                           NDATA, MXCOL, AVE, SDEV,
     :                           YERRORARRAY(SLOT),
     :                           %VAL(CNF_PVAL(YSLOT2)))

            DETRENDARRAY(SLOTOUT) = .TRUE.
            YERRORARRAY(SLOTOUT) = YERRORARRAY(SLOT)
            NPTSARRAY(SLOTOUT) = NDATA
            INFILEARRAY(SLOTOUT) = 'Detrended ' // INFILEARRAY(SLOT)
         END IF

         WRITE (*, *) '** OK: Filled slot = ', SLOTOUT

 300  CONTINUE

 400  CONTINUE
      RETURN
      END
