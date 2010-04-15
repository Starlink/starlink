
      SUBROUTINE PERIOD_PHASE(YPTR, MXCOL, MXSLOT, NPTSARRAY,
     :                        DETRENDARRAY, INFILEARRAY, YERRORARRAY)

C===========================================================================
C Folds and bins data on a given PERIOD with a zero point defined by ZEROPT.
C
C Written by Vikram Dhillon @Sussex 20-March-1992.
C
C GJP March 1997
C Replaced ZEROPT.EQ.0 statement.
C
C Converted to Double Precision (KPD), August 2001
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
C PERIOD_PHASE declarations.
C-----------------------------------------------------------------------------

      INTEGER NDATA, NBIN
      DOUBLE PRECISION PERIOD, ZEROPT
      DOUBLE PRECISION PERIOD_GET2D
      INTEGER SLOT, FIRSTSLOT, LASTSLOT, FIRSTOUT, IFAIL, SLOTOUT
      INTEGER COUNTER
      INTEGER XDATAPTR, YDATAPTR, YERRPTR
      INTEGER XBINPTR, YBINPTR, EBINPTR
      INTEGER YSLOT1, YSLOT2
      LOGICAL DETRENDARRAY(MXSLOT), YERRORARRAY(MXSLOT)
      CHARACTER*72 INFILEARRAY(MXSLOT)


C-----------------------------------------------------------------------------
C Select input and output data slots.
C-----------------------------------------------------------------------------

      WRITE (*, *) ' '
      CALL PERIOD_SELECT(FIRSTSLOT, LASTSLOT, FIRSTOUT, MXSLOT, IFAIL)
      IF ( IFAIL.EQ.1 ) GO TO 600

C-----------------------------------------------------------------------------
C Prompt for PERIOD and ZEROPT parameters.
C-----------------------------------------------------------------------------

      WRITE (*, *) ' '
 100  CONTINUE
      WRITE (*, '(X,A,$)') 'Enter period to fold data on : '
      READ (*, *, ERR=100) PERIOD
      IF ( PERIOD.LE.0. ) GO TO 100
 200  CONTINUE
      WRITE (*, '(X,A,$)') 'Enter zero point (0 for first' //
     :                     ' data point) : '
      READ (*, *, ERR=200) ZEROPT
 300  CONTINUE
      WRITE (*, '(X,A,$)') 'Enter number of phase bins (0 for simple' //
     :                     ' folding) : '
      READ (*, *, ERR=300) NBIN
      WRITE (*, *) ' '

      COUNTER = 0

      DO 500 SLOT = FIRSTSLOT, LASTSLOT
         NDATA = NPTSARRAY(SLOT)

         IF ( NDATA.EQ.0 ) THEN
            CALL PERIOD_WRITEBELL()
            WRITE (*, *) '** ERROR: Slot empty =', SLOT
            GO TO 700
         END IF

         SLOTOUT = FIRSTOUT + COUNTER

         IF ( SLOT.EQ.SLOTOUT ) THEN
            IF ( NBIN .GT.NDATA ) THEN
               CALL PERIOD_WRITEBELL()
               WRITE (*, *) '** ERROR: Number of phase bins ' //
     :                      'should not exceed ', NDATA
               GO TO 700
            END IF
         ELSE
            IF ( NPTSARRAY(SLOTOUT).NE.0 )
     :         CALL PERIOD_DEALL(YPTR(SLOTOUT))
            CALL PERIOD_ALLOC('_DOUBLE', NDATA*MXCOL, YPTR(SLOTOUT))
         END IF

         YSLOT1 = YPTR(SLOT)
         YSLOT2 = YPTR(SLOTOUT)

         COUNTER = COUNTER + 1

C        IF ( DABS(ZEROPT).LT.DPMN30 ) ZEROPT = Y(1,1,SLOT)

         IF ( DABS(ZEROPT).LT.DPMN30 )
     :      ZEROPT = PERIOD_GET2D(1, 1, %VAL(CNF_PVAL(YSLOT1)),
     :                            NDATA, MXCOL)

C-----------------------------------------------------------------------------
C Fold and sort data.
C-----------------------------------------------------------------------------

         CALL PERIOD_ALLOC('_DOUBLE', NDATA, XDATAPTR)
         CALL PERIOD_ALLOC('_DOUBLE', NDATA, YDATAPTR)
         CALL PERIOD_ALLOC('_DOUBLE', NDATA, YERRPTR)

         CALL PERIOD_SETXYERR(%VAL(CNF_PVAL(YSLOT1)), NDATA, MXCOL,
     :                        %VAL(CNF_PVAL(XDATAPTR)),
     :                        %VAL(CNF_PVAL(YDATAPTR)),
     :                        %VAL(CNF_PVAL(YERRPTR)))

         CALL PERIOD_FOLD(%VAL(CNF_PVAL(XDATAPTR)),
     :                    %VAL(CNF_PVAL(YDATAPTR)),
     :                    %VAL(CNF_PVAL(YERRPTR)),
     :                    NDATA, ZEROPT, PERIOD,
     :                    IFAIL)

         IF ( IFAIL.EQ.1 ) GO TO 600

C-----------------------------------------------------------------------------
C Bin data in phase bins (if NBIN = 0, leave data unchanged).
C-----------------------------------------------------------------------------

         IF ( NBIN.GT.1 ) THEN

            CALL PERIOD_ALLOC('_DOUBLE', NBIN, XBINPTR)
            CALL PERIOD_ALLOC('_DOUBLE', NBIN, YBINPTR)
            CALL PERIOD_ALLOC('_DOUBLE', NBIN, EBINPTR)

            CALL PERIOD_PHASEBIN(%VAL(CNF_PVAL(XDATAPTR)),
     :                           %VAL(CNF_PVAL(YDATAPTR)),
     :                           %VAL(CNF_PVAL(YERRPTR)), NDATA,
     :                           %VAL(CNF_PVAL(XBINPTR)),
     :                           %VAL(CNF_PVAL(YBINPTR)),
     :                           %VAL(CNF_PVAL(EBINPTR)), NBIN,
     :                           %VAL(CNF_PVAL(YSLOT2)), MXCOL)

            YERRORARRAY(SLOTOUT) = YERRORARRAY(SLOT)
            DETRENDARRAY(SLOTOUT) = .FALSE.
            INFILEARRAY(SLOTOUT) = 'Binned ' // INFILEARRAY(SLOT)
            NPTSARRAY(SLOTOUT) = NBIN
            WRITE (*, *) '** OK: Filled slot = ', SLOTOUT

            CALL PERIOD_DEALL(EBINPTR)
            CALL PERIOD_DEALL(YBINPTR)
            CALL PERIOD_DEALL(XBINPTR)

         ELSE

            CALL PERIOD_PUTXYERR(%VAL(CNF_PVAL(XDATAPTR)),
     :                           %VAL(CNF_PVAL(YDATAPTR)),
     :                           %VAL(CNF_PVAL(YERRPTR)),
     :                           %VAL(CNF_PVAL(YSLOT2)),
     :                           NDATA, MXCOL)

            YERRORARRAY(SLOTOUT) = YERRORARRAY(SLOT)
            DETRENDARRAY(SLOTOUT) = .FALSE.
            INFILEARRAY(SLOTOUT) = 'Folded ' // INFILEARRAY(SLOT)
            NPTSARRAY(SLOTOUT) = NDATA
            WRITE (*, *) '** OK: Filled slot = ', SLOTOUT
         END IF

 500  CONTINUE

 600  CONTINUE
      CALL PERIOD_DEALL(YERRPTR)
      CALL PERIOD_DEALL(YDATAPTR)
      CALL PERIOD_DEALL(XDATAPTR)

 700  CONTINUE
      RETURN

      END
