C-----------------------------------------------------------------------

      SUBROUTINE FNDWID (NQ, XSCALE, BUF, IFAIL)

*  Routine to evaluate equivalent width of a line

      IMPLICIT   NONE

*     Formal parameters

      INTEGER    NQ
      REAL       XSCALE(*)
      REAL       BUF(*)
      INTEGER    IFAIL

*     Local variables

      INTEGER    IHALF(2)
      INTEGER    J
      INTEGER    NST
      INTEGER    PAIRS
      INTEGER    NXPTS
      REAL       AINTEG
      REAL       FRAC
      REAL       POSMAX
      REAL       VALMAX
      REAL       XWHM

*     Include files

      INCLUDE   'FLAGCOMM'
      INCLUDE   'STACKCOMM'

*     Functions

      INTEGER    ISLCT1Q
      INTEGER    NTOT

*  Ok, go...

      IFAIL = 0

      CALL SETXNEW (XSCALE, IFAIL)
      IF (IFAIL.NE.0)   RETURN

      NQ = ISLCT1Q (NQ, IFAIL)
      IF (IFAIL.NE.0)   RETURN

*     switch into quadrant NQ

      NST = NTOT(NQ-1)

*     Find a window of interest

      PAIRS = 1
      CALL GETPTS (IHALF, 1, 1, PAIRS, PAIRS, XSCALE(NST+1),
     &             DATA(NST+1), NPTS(NQ), IFAIL)
      IF (IFAIL.NE.0) RETURN

*     Find maximum in window

      NXPTS = IHALF(2) - IHALF(1) + 1
      CALL FINDMX (NXPTS, POSMAX, DATA(NST+IHALF(1)),
     &             BADPIX_VAL, VALMAX)

*     Integrate to get total intensity

      AINTEG = 0.0
      DO J = IHALF(1), IHALF(2)
        IF (J.EQ.IHALF(1)) THEN
          FRAC = 1.0
        ELSE IF (J.EQ.IHALF(2)) THEN
          FRAC = 1.0
        ELSE
          FRAC = 1.0
        END IF
        IF (DATA(NST+J-1).NE.BADPIX_VAL) THEN
          AINTEG = AINTEG + DATA(NST+J-1)*FRAC
        END IF
      END DO

*     Give in terms of current X-units

      AINTEG = AINTEG * ABS (XFAC(NQ))

*     Calculate equivalent width

      XWHM   = AINTEG / VALMAX

      WRITE(ILOUT,1000) NQ, XWHM, XAXIS_UNITS
 1000 FORMAT(/' Quadrant 'I2'  Equivalent line width '
     &       F7.2' 'A6)

      RETURN
      END


