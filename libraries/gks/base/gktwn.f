C# IL>=b, OL>=0
      SUBROUTINE GKTWN(IT,N,XWC,YWC,XNDC,YNDC)
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
*     Transform from WC to NDC for input.
*
*  MAINTENANCE LOG
*  ---------------
*     11/08/83  AS    Original version stabilized
*     02/12/83  AS    Add a comment
*
*  ARGUMENTS
*  ---------
*     INP  IT      Transformation number
*     INP  N       Number of points
*     INP  XWC     World coordinate
*     INP  YWC     World coordinate
*     OUT  XNDC    Normalized device coordinate
*     OUT  YNDC    Normalized device coordinate
*
      INTEGER IT, N
      REAL XWC(N), YWC(N), XNDC(N), YNDC(N)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*
      INTEGER I, J, ITNR
*
*  ERRORS
*  ------
*     152   Value invalid
*
*---------------------------------------------------------------------



* Transform WC to NDC

      DO 10 I=1,N
        XNDC(I) = (QLVPXR(IT) * (XWC(I)-QLWXL(IT)) +
     :             QLVPXL(IT) * (QLWXR(IT)-XWC(I)) )
     :             /(QLWXR(IT)-QLWXL(IT))
        YNDC(I) = (QLVPYT(IT) * (YWC(I)-QLWYB(IT)) +
     :             QLVPYB(IT) * (QLWYT(IT)-YWC(I)) )
     :             /(QLWYT(IT)-QLWYB(IT))
   10 CONTINUE

* Find out highest priority viewport which all points lie in
      DO 30 I=0,KT
        ITNR = KTNOVP(I)
        DO 20 J=1,N
        IF (XNDC(J).LT.QLVPXL(ITNR) .OR. XNDC(J).GT.QLVPXR(ITNR) .OR.
     :      YNDC(J).LT.QLVPYB(ITNR) .OR. YNDC(J).GT.QLVPYT(ITNR))GOTO 30
   20   CONTINUE
        GOTO 40
   30 CONTINUE

   40 CONTINUE
* This viewport should have the same transformation number as the one
* supplied
      IF (ITNR.NE.IT) KERROR = 152

      END
