*-----------------------------------------------------------------------

      SUBROUTINE CENTRD (NQ, XSCALE, BUF, IFAIL)

C   Routine to calculate centroid of area of a line profile

C   History:
C      6-JUN-2000 (AJC):
C        Missing commas in FORMAT
C        Initialise FRAC so Linux behaves the same as other platforms

      IMPLICIT  NONE

*     Formal parameters

      INTEGER   NQ
      REAL      XSCALE(*)
      REAL      BUF(*)
      INTEGER   IFAIL

*     Common blocks

      INCLUDE  'STACKCOMM'
      INCLUDE  'FLAGCOMM'

*     Local variables

      INTEGER   J
      INTEGER   NP
      INTEGER   NST
      REAL      AREA
      REAL      FRAC
      REAL      FRAC1

      INTEGER   NC(2)
      EQUIVALENCE    (NC(1),NC1)

*     Functions

      LOGICAL   DOQUAD
      INTEGER   NTOT
      REAL      XTRANS

*  Ok, go...

      FRAC = 0.0

      NST = NTOT(NQ-1)+1
      CALL SETXNEW (XSCALE, IFAIL)
      IF (IFAIL.NE.0) RETURN

      NP  = 1
      CALL GETPTS (NC, 1, 1, NP, NP, XSCALE(NST), DATA(NST),
     &             NPTS(NQ), IFAIL)
      IF (IFAIL.NE.0) RETURN

      IF (DOQUAD(NQ)) THEN

        NST   = NTOT (NQ-1)
        AREA  = 0.
        FRAC1 = 0.

        DO J = NC1+NST, NC2+NST
          IF (DATA(J).NE.BADPIX_VAL) THEN
            AREA = AREA + DATA(J)
          END IF
        END DO

        DO J = NC1+NST, NC2+NST
          IF (DATA(J).NE.BADPIX_VAL) THEN
            FRAC = (FRAC1*AREA+DATA(J)) / AREA
            IF (FRAC.GT.0.5)   GO TO 35
            FRAC1 = FRAC
          END IF
        END DO

   35   XCHAN = FLOAT(J-NST) - (FRAC-0.5) / (FRAC-FRAC1) + 0.5
        XCHAN = XTRANS (XSCALE(NST+1), XCHAN, NPTS(NQ), IFAIL)
        WRITE (ILOUT,1000) NQ, XCHAN, XAXIS_UNITS

      END IF
      RETURN

 1000 FORMAT(/'  Quadrant ',I1,' : Centroid is at ',F7.2,1X,A6)

      END

*-----------------------------------------------------------------------
