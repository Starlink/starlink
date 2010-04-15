
      SUBROUTINE PERIOD_FOLD(XDATA, YDATA, YERR, NDATA, ZEROPT, PERIOD,
     :                       IFAIL)

C===========================================================================
C Folds data XDATA(NDATA) on PERIOD about ZEROPT.
C
C Written by Vikram Singh Dhillon @Sussex 29-March-1992.
C
C Converted to Double Precision (KPD), August 2001
C Modified to incorporate dynamic memory allocation for major
C  data/work array(s) and/or use of such arrays (KPD), October 2001
C===========================================================================

      IMPLICIT NONE

      INCLUDE 'CNF_PAR'

      INTEGER NDATA
      DOUBLE PRECISION XDATA(NDATA), YDATA(NDATA), YERR(NDATA)
      DOUBLE PRECISION ZEROPT, PERIOD
      INTEGER IFAIL
      INTEGER XWORKPTR, YWORKPTR, EWORKPTR, KEYPTR

C-----------------------------------------------------------------------------
C Dynamically memory for work arrays.
C-----------------------------------------------------------------------------

      IFAIL = 0

      CALL PERIOD_ALLOC('_DOUBLE', NDATA, XWORKPTR)
      CALL PERIOD_ALLOC('_DOUBLE', NDATA, YWORKPTR)
      CALL PERIOD_ALLOC('_DOUBLE', NDATA, EWORKPTR)
      CALL PERIOD_ALLOC('_INTEGER', NDATA, KEYPTR)


C-----------------------------------------------------------------------------
C Fold data.
C-----------------------------------------------------------------------------

      CALL PERIOD_FOLDSORTCYCLE(XDATA, YDATA, YERR, NDATA,
     :                          %VAL(CNF_PVAL(XWORKPTR)),
     :                          %VAL(CNF_PVAL(YWORKPTR)),
     :                          %VAL(CNF_PVAL(EWORKPTR)),
     :                          %VAL(CNF_PVAL(KEYPTR)),
     :                          ZEROPT, PERIOD)

      CALL PERIOD_DEALL(KEYPTR)
      CALL PERIOD_DEALL(EWORKPTR)
      CALL PERIOD_DEALL(YWORKPTR)
      CALL PERIOD_DEALL(XWORKPTR)

      RETURN
      END
