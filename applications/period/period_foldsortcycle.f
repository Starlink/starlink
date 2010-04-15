
      SUBROUTINE PERIOD_FOLDSORTCYCLE(XDATA, YDATA, YERR, NDATA,
     :                                XWORK, YWORK, EWORK, KEY,
     :                                ZEROPT, PERIOD)

C===========================================================================
C Folds data XDATA(NDATA) on PERIOD about ZEROPT.
C
C Written by Kevin P Duffey @RAL, October 2001
C
C Converted to Double Precision (KPD), August 2001
C Modified to incorporate dynamic memory allocation for major
C  data/work array(s) and/or use of such arrays (KPD), October 2001
C===========================================================================

      IMPLICIT NONE

      INTEGER NDATA
      INTEGER KEY(NDATA)
      DOUBLE PRECISION XDATA(NDATA), YDATA(NDATA), YERR(NDATA)
      DOUBLE PRECISION XWORK(NDATA), YWORK(NDATA), EWORK(NDATA)
      DOUBLE PRECISION ZEROPT, PERIOD
      INTEGER I


C-----------------------------------------------------------------------------
C Load work arrays and fold data.
C-----------------------------------------------------------------------------

      DO 10 I = 1, NDATA

         YWORK(I) = YDATA(I)
         EWORK(I) = YERR(I)

         XWORK(I) = ((XDATA(I)-ZEROPT)/PERIOD)
     :              - DINT((XDATA(I)-ZEROPT)/PERIOD)
         IF ( XWORK(I).LE.0.0D0 ) XWORK(I) = XWORK(I) + 1.0D0

  10  CONTINUE

C-----------------------------------------------------------------------------
C Sort data into ascending order.
C-----------------------------------------------------------------------------

      CALL PERIOD_SHELLSORT(NDATA, XWORK, KEY)

      DO 20 I = 1, NDATA
         XDATA(I) = XWORK(KEY(I))
         YDATA(I) = YWORK(KEY(I))
         YERR(I) =  EWORK(KEY(I))
  20  CONTINUE

      RETURN
      END
