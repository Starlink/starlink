
      SUBROUTINE PERIOD_FIT(YPTR, MXCOL, MXSLOT, NPTSARRAY,
     :                      DETRENDARRAY, YERRORARRAY, INFILEARRAY,
     :                      LOG, LOGUNIT)

C===========================================================================
C Folds data on a given PERIOD with a zero point defined by ZEROPT and
C then fits a sine curve to the data.
C
C Written by Vikram Singh Dhillon @Sussex 20-March-1992.
C
C GJP June 1995
C
C Unused parameter LOGFILE removed.
C
C GJP March 1997
C
C Replaced ZEROPT.EQ.0. Modified INTEGER *4 refs.
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
C PERIOD_FIT declarations.
C-----------------------------------------------------------------------------

      INTEGER NDATA, NUMPTS
      PARAMETER (NUMPTS=1000)
      DOUBLE PRECISION GAMMA, KVEL, PHASE, VAR(6), F, HJD0
      DOUBLE PRECISION PERIOD, ZEROPT
      DOUBLE PRECISION PERROR, HJD0ERR, ERRPHPE, ZERROR
      DOUBLE PRECISION PERIOD_GET2D
      INTEGER SLOT, FIRSTSLOT, LASTSLOT, FIRSTOUT, IFAIL, SLOTOUT
      INTEGER NP, LOGUNIT, COUNTER
      INTEGER XDATAPTR, YDATAPTR, YERRPTR
      INTEGER YSLOT1, YSLOT2
      LOGICAL DETRENDARRAY(MXSLOT), YERRORARRAY(MXSLOT), LOG
      CHARACTER*1 REPLY
      CHARACTER*72 INFILEARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C Select input and output data slots.
C-----------------------------------------------------------------------------

      WRITE (*, *) ' '
      CALL PERIOD_SELECT(FIRSTSLOT, LASTSLOT, FIRSTOUT, MXSLOT, IFAIL)
      IF ( IFAIL.EQ.1 ) GO TO 800

C-----------------------------------------------------------------------------
C Prompt for PERIOD and ZEROPT parameters.
C-----------------------------------------------------------------------------

      WRITE (*, *) ' '
 100  CONTINUE
      WRITE (*, '(X,A,$)') 'Enter period to fold data on : '
      READ (*, *, ERR=100) PERIOD
      IF ( PERIOD.LE.0.0D0 ) GO TO 100
 200  CONTINUE
      WRITE (*, '(X,A,$)') 'Enter error in period : '
      READ (*, *, ERR=200) PERROR
      IF ( PERROR.LT.0.0D0 ) GO TO 200
 300  CONTINUE
      WRITE (*, '(X,A,$)')
     :                    'Enter zero point (0 for first data point) : '
      READ (*, *, ERR=300) ZEROPT
 400  CONTINUE
      WRITE (*, '(X,A,$)') 'Enter error in zero point : '
      READ (*, *, ERR=400) ZERROR
      IF ( ZERROR.LT.0.0D0 ) GO TO 400

      COUNTER = 0

      DO 700 SLOT = FIRSTSLOT, LASTSLOT
         NDATA = NPTSARRAY(SLOT)

         IF ( NDATA.EQ.0 ) THEN
            CALL PERIOD_WRITEBELL()
            WRITE (*, *) '** ERROR: Slot empty =', SLOT
            GO TO 800
         END IF

         SLOTOUT = FIRSTOUT + COUNTER
         COUNTER = COUNTER + 1
         YSLOT1 = YPTR(SLOT)

C        IF ( DABS(ZEROPT).LT.DPMN30 ) ZEROPT=Y(1,1,SLOT)

         IF ( DABS(ZEROPT).LT.DPMN30 )
     :      ZEROPT = PERIOD_GET2D(1, 1, %VAL(CNF_PVAL(YSLOT1)),
     :                            NDATA, MXCOL)

         CALL PERIOD_ALLOC('_DOUBLE', NDATA, XDATAPTR)
         CALL PERIOD_ALLOC('_DOUBLE', NDATA, YDATAPTR)
         CALL PERIOD_ALLOC('_DOUBLE', NDATA, YERRPTR)

C-----------------------------------------------------------------------------
C Fold and sort data.
C-----------------------------------------------------------------------------

         CALL PERIOD_SETXYIFERR(%VAL(CNF_PVAL(YSLOT1)), NDATA, MXCOL,
     :                          YERRORARRAY(SLOT),
     :                          %VAL(CNF_PVAL(XDATAPTR)),
     :                          %VAL(CNF_PVAL(YDATAPTR)),
     :                          %VAL(CNF_PVAL(YERRPTR)))

         CALL PERIOD_FOLD(%VAL(CNF_PVAL(XDATAPTR)),
     :                    %VAL(CNF_PVAL(YDATAPTR)),
     :                    %VAL(CNF_PVAL(YERRPTR)),
     :                    NDATA, ZEROPT, PERIOD, IFAIL)

         IF ( IFAIL.EQ.1 ) GO TO 650

C-----------------------------------------------------------------------------
C Fit data.
C-----------------------------------------------------------------------------

         CALL PERIOD_SINFIT(%VAL(CNF_PVAL(XDATAPTR)),
     :                      %VAL(CNF_PVAL(YDATAPTR)),
     :                      %VAL(CNF_PVAL(YERRPTR)),
     :                      NDATA, 1.0D0, GAMMA, KVEL,
     :                      PHASE, VAR, NP, F, IFAIL)

         IF ( IFAIL.EQ.1 ) THEN
            CALL PERIOD_WRITEBELL()
            WRITE (*, *) '** ERROR: Sine fit unsuccessful.'
            GO TO 650
         END IF

         HJD0 = ZEROPT + (PHASE*PERIOD)
         ERRPHPE = DSQRT(((PHASE*PERIOD)**2)
     :             *((VAR(6)/(PHASE**2))+((PERROR**2)/(PERIOD**2))))
         HJD0ERR = DSQRT((ZERROR**2)+(ERRPHPE**2))
         WRITE (*, *) ' '
         WRITE (*, *) '** OK: Period = ', PERIOD
         WRITE (*, *) '** OK: Sigma period = ', PERROR
         WRITE (*, *) '** OK: Reduced chi-squared = ',
     :                                          F/(DFLOAT(NP)-3.0D0)
         WRITE (*, *) '** OK: Number of points = ', NP
         WRITE (*, *) '** OK: Gamma = ', GAMMA
         WRITE (*, *) '** OK: Sigma gamma = ', DSQRT(VAR(1))
         WRITE (*, *) '** OK: Semi-amplitude = ', KVEL
         WRITE (*, *) '** OK: Sigma semi-amplitude = ', DSQRT(VAR(4))
         WRITE (*, *) '** OK: Phi0 = ', PHASE
         WRITE (*, *) '** OK: Sigma phi0 = ', DSQRT(VAR(6))
         WRITE (*, *) '** OK: Original zero point = ', ZEROPT
         WRITE (*, *) '** OK: Revised zero point = ', HJD0
         WRITE (*, *) '** OK: Error in revised zero point = ', HJD0ERR
         WRITE (*, *) ' '
 500     CONTINUE
         WRITE (*, '(X,A,$)') 'Do you want to log this fit ? [N] : '
         READ (*, '(A)', ERR=500) REPLY
         CALL PERIOD_CASE(REPLY, .TRUE.)
         IF ( REPLY.EQ.'Y' ) THEN
            IF ( .NOT.LOG ) THEN
               CALL PERIOD_WRITEBELL()
               WRITE (*, *) '** ERROR: No log file has been opened.'
               GO TO 550
            END IF
            WRITE (LOGUNIT, *) ' '
            WRITE (LOGUNIT, *) '** OK: Sinusoidal Fit To ' //
     :                         INFILEARRAY(SLOT)(1:47)
            WRITE (LOGUNIT, *) ' '
            WRITE (LOGUNIT, *) '** OK: Period = ', PERIOD
            WRITE (LOGUNIT, *) '** OK: Sigma period = ', PERROR
            WRITE (LOGUNIT, *) '** OK: Reduced chi-squared = ',
     :                         F/(DFLOAT(NP)-3.0D0)
            WRITE (LOGUNIT, *) '** OK: Number of points = ', NP
            WRITE (LOGUNIT, *) '** OK: Gamma = ', GAMMA
            WRITE (LOGUNIT, *) '** OK: Sigma gamma = ', DSQRT(VAR(1))
            WRITE (LOGUNIT, *) '** OK: Semi-amplitude = ', KVEL
            WRITE (LOGUNIT, *) '** OK: Sigma semi-amplitude = ',
     :                         DSQRT(VAR(4))
            WRITE (LOGUNIT, *) '** OK: Phi0 = ', PHASE
            WRITE (LOGUNIT, *) '** OK: Sigma phi0 = ', DSQRT(VAR(6))
            WRITE (LOGUNIT, *) '** OK: Original zero point = ', ZEROPT
            WRITE (LOGUNIT, *) '** OK: Revised zero point = ', HJD0
            WRITE (LOGUNIT, *) '** OK: Error in revised zero point = ',
     :                         HJD0ERR
            WRITE (LOGUNIT, '(A)') '.'
         END IF

C-----------------------------------------------------------------------------
C Load output slot.
C-----------------------------------------------------------------------------

 550     CONTINUE
         NDATA = NUMPTS

         IF (NPTSARRAY(SLOTOUT) .NE. 0)
     :      CALL PERIOD_DEALL(YPTR(SLOTOUT))
         CALL PERIOD_ALLOC('_DOUBLE', NDATA*MXCOL, YPTR(SLOTOUT))
         YSLOT2 = YPTR(SLOTOUT)

C NOTE: In period_putfitdata, original loop "0,NDATA" has been amended
C       by KPD to be "1,NDATA" - to avoid array-bound exception!

         CALL PERIOD_PUTFITDATA(%VAL(CNF_PVAL(YSLOT2)),
     :                          NDATA, MXCOL, GAMMA,
     :                          KVEL, PHASE)

         YERRORARRAY(SLOTOUT) = .FALSE.
         DETRENDARRAY(SLOTOUT) = .FALSE.
         NPTSARRAY(SLOTOUT) = NDATA
         INFILEARRAY(SLOTOUT) = 'Sinusoidal Fit To ' //
     :                          INFILEARRAY(SLOT)
         WRITE (*, *) ' '
         WRITE (*, *) '** OK: Filled slot = ', SLOTOUT
         WRITE (*, *) ' '

 650     CALL PERIOD_DEALL(YERRPTR)
         CALL PERIOD_DEALL(YDATAPTR)
         CALL PERIOD_DEALL(XDATAPTR)

         IF ( IFAIL.EQ.1 ) GO TO 800

 700  CONTINUE

 800  CONTINUE
      RETURN
      END
