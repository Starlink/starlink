C# IL>=b, OL>=0
      SUBROUTINE GKTNW(N,XNDC,YNDC,XWC,YWC,IT)
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
*     Transform from NDC to WC
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
*     OUT  XWC     World coordinate
*     OUT  YWC     World coordinate
*     OUT  IT      Transformation number
*
      INTEGER N, IT
      REAL XNDC(N), YNDC(N), XWC(N), YWC(N)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gksl.cmn'
*
*  LOCALS
*  ------
*
      INTEGER I, J
*
*---------------------------------------------------------------------



* Find normalization transformation number

      DO 20 J=0,KT
        IT = KTNOVP(J)
        DO 10 I=1,N
        IF (XNDC(I).LT.QLVPXL(IT) .OR. XNDC(I).GT.QLVPXR(IT) .OR.
     :      YNDC(I).LT.QLVPYB(IT) .OR. YNDC(I).GT.QLVPYT(IT)) GOTO 20
   10   CONTINUE
        GOTO 30
   20 CONTINUE

* Transform NDC to WC

   30 CONTINUE
      DO 40 I=1,N
        XWC(I) = (QLWXR(IT) * (XNDC(I)-QLVPXL(IT)) +
     :            QLWXL(IT) * (QLVPXR(IT)-XNDC(I)) )
     :            /(QLVPXR(IT)-QLVPXL(IT))
        YWC(I) = (QLWYT(IT) * (YNDC(I)-QLVPYB(IT)) +
     :            QLWYB(IT) * (QLVPYT(IT)-YNDC(I)) )
     :            /(QLVPYT(IT)-QLVPYB(IT))
   40 CONTINUE


      END
