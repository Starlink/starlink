C History:
C   01-Aug-1995 (rpt):
C    ICOLOR support added.
C
C-----------------------------------------------------------------------

      SUBROUTINE QPLOT (XSCALE, IWEIGHT, LCOLOR, IFAIL)

      IMPLICIT   NONE

C     Formal parameters:

      REAL       XSCALE(1)
      INTEGER    IWEIGHT
      INTEGER    LCOLOR
      INTEGER    IFAIL

C     Include files:

      INCLUDE   'NOKEEP'
      INCLUDE   'STACKCOMM'
      INCLUDE   'FLAGCOMM'

C     Local variables:

      INTEGER   ITIP
      LOGICAL   TOPSCAL
      CHARACTER STRING*80

C  Ok, go...

      IF (JPLOT.EQ.0)  THEN
        IFAIL = 7
        RETURN
      ELSE
        IF (HISTOGRAM) THEN
          ITIP = 1
        ELSE
          ITIP = 0
        END IF

        IPEN = 0
        CALL SETXNEW (XSCALE, IFAIL)
        IF (IFAIL.NE.0) RETURN

        TOPSCAL = (NXS.EQ.2) .and. ABS_FREQ
        CALL PLTDAT  (NPTS, NQUAD, ITIP, IWEIGHT, LCOLOR,
     &                JPLOT, TOPSCAL, XSCALE, DATA)
      END IF

      RETURN
      END

C-----------------------------------------------------------------------

