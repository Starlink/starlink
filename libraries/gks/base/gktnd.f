C# IL>=b, OL>=0
      SUBROUTINE GKTND(N,XNDC,YNDC,XDC,YDC)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Transform from NDC to DC
*
*  MAINTENANCE LOG
*  ---------------
*     11/08/83  AS    Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP  N       Number of points
*     INP  XNDC    Normalized device coordinate
*     INP  YNDC    Normalized device coordinate
*     OUT  XDC     Device coordinate
*     OUT  YDC     Device coordinate
*
      INTEGER N
      REAL XNDC(N), YNDC(N), XDC(N), YDC(N)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwsl.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*
      INTEGER I
      REAL WVX, WVY, WWX, WWY, SF
*
*  ERRORS
*  ------
*     152  Value invalid
*
*---------------------------------------------------------------------



      WVX = QCWVXR(KWKIX)-QCWVXL(KWKIX)
      WVY = QCWVYT(KWKIX)-QCWVYB(KWKIX)
      WWX = QCWWXR(KWKIX)-QCWWXL(KWKIX)
      WWY = QCWWYT(KWKIX)-QCWWYB(KWKIX)
      IF (WWX * WVY .GT. WWY * WVX) THEN
         SF = WVX / WWX
      ELSE
         SF = WVY / WWY
      ENDIF

      DO 10 I=1,N
* Check point is within workstation window
        IF (XNDC(I).LT.QCWWXL(KWKIX) .OR. XNDC(I).GT.QCWWXR(KWKIX) .OR.
     :      YNDC(I).LT.QCWWYB(KWKIX) .OR. YNDC(I).GT.QCWWYT(KWKIX))
     :  KERROR = 152
        XDC(I) = (XNDC(I)-QCWWXL(KWKIX)) * SF  +  QCWVXL(KWKIX)
        YDC(I) = (YNDC(I)-QCWWYB(KWKIX)) * SF  +  QCWVYB(KWKIX)
   10 CONTINUE

      END
