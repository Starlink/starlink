C# IL>=a, OL>=0
      SUBROUTINE GKROTV(IVERT,XIN,YIN,ANG,XOUT,YOUT)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             NGB
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Rotates the set of vertices in X/YIN by ANG to X/YOUT,
*     in preparation for scan converting angled hatch lines.
*     leaving the inverse transform coefficients in QWR7/8
*
*  MAINTENANCE LOG
*  ---------------
*     01/02/83  NGB   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP IVERT   No of Vertices
*     INP XIN,YIN Input Vertex Coordinates
*     INP ANG     Rotation Angle
*     OUT XOUT,YOUT Output Vertex Coordinates
*
      INTEGER IVERT
      REAL XIN(IVERT), YIN(IVERT), ANG, XOUT(IVERT), YOUT(IVERT)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwca.cmn'
*
*  LOCALS
*  ------
*
      INTEGER IV
*
*---------------------------------------------------------------------



* Set Hatch Rotation Coefficients
      QWR7=SIN(ANG)
      QWR8=COS(ANG)
* Rotate Vertices
      DO 100  IV=1,IVERT
         XOUT(IV)=XIN(IV)*QWR8 - YIN(IV)*QWR7
         YOUT(IV)=XIN(IV)*QWR7 + YIN(IV)*QWR8
  100 CONTINUE

* Set Hatch Inverse Rotation Coefficients
      QWR7=SIN(-ANG)
      QWR8=COS(-ANG)

      END
