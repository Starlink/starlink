C# IL>=b, OL>=0
      SUBROUTINE GKTDN(N,XDC,YDC,XNDC,YNDC)
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
*     Transform from DC to NDC
*
*  MAINTENANCE LOG
*  ---------------
*     11/08/83  AS    Original version stabilized.
*     21/01/87  ARG   IS conversion. Error number corrected in header.
*
*  ARGUMENTS
*  ---------
*     INP  N       Number of points
*     INP  XDC     Device coordinate
*     INP  YDC     Device coordinate
*     OUT  XNDC    Normalized device coordinate
*     OUT  YNDC    Normalized device coordinate
*
      INTEGER N
      REAL XDC(N), YDC(N), XNDC(N), YNDC(N)
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
*     152  Initial value invalid
*---------------------------------------------------------------------



      WVX = QCWVXR(KWKIX)-QCWVXL(KWKIX)
      WVY = QCWVYT(KWKIX)-QCWVYB(KWKIX)
      WWX = QCWWXR(KWKIX)-QCWWXL(KWKIX)
      WWY = QCWWYT(KWKIX)-QCWWYB(KWKIX)
      IF (WWX * WVY .GT. WWY * WVX) THEN
         SF = WWX / WVX
      ELSE
         SF = WWY / WVY
      ENDIF

      DO 10 I=1,N
        XNDC(I) = (XDC(I)-QCWVXL(KWKIX)) * SF  +  QCWWXL(KWKIX)
        YNDC(I) = (YDC(I)-QCWVYB(KWKIX)) * SF  +  QCWWYB(KWKIX)
* Check point is within workstation window
        IF (XNDC(I).LT.QCWWXL(KWKIX) .OR. XNDC(I).GT.QCWWXR(KWKIX) .OR.
     :      YNDC(I).LT.QCWWYB(KWKIX) .OR. YNDC(I).GT.QCWWYT(KWKIX))
     :  KERROR = 152
   10 CONTINUE

      END
