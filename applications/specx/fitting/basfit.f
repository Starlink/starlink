C-----------------------------------------------------------------------

      SUBROUTINE BASFIT (NQ, XSCALE, BUF, IFAIL)

      IMPLICIT  NONE

*     Formal parameters

      INTEGER  NQ
      REAL     XSCALE(*)
      REAL     BUF(*)
      INTEGER  IFAIL

*     Common blocks

      INCLUDE 'FLAGCOMM'
      INCLUDE 'STACKCOMM'

*     Local variables

      INTEGER  I
      INTEGER  NOLD
      INTEGER  NP
      INTEGER  NST

*     Functions

      INTEGER  NTOT

*  Ok, go...

      TYPE *, 'Doing R-L-B for quadrant/sub-band # ', NQ

      CALL SETXNEW (XSCALE, IFAIL)
      IF (IFAIL.NE.0) RETURN

      NOLD = 2
      NP   = 0
      NST  = NTOT(NQ-1)+1
      CALL GETPTS (IBLRPT, 2, 2, NOLD, NP, XSCALE(NST),
     &             DATA(NST), NPTS, IFAIL)

      DO I = 1, 4
        IF (IBLRPT(I).LT.1 .OR. IBLRPT(I).GT.NPTS(NQ)) IFAIL = 19
      END DO

      IF (IFAIL.EQ.0) THEN
        CALL COPYBF (NQ, BUF)
        CALL BASLIN (BUF, DATA(NTOT(NQ-1)+1), NPTS(NQ),
     &               IBLRPT, BADPIX_VAL, IFAIL)
      END IF
      RETURN

      END

*-----------------------------------------------------------------------
