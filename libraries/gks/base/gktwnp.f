      SUBROUTINE GKTWNP(IT,N,XWC,YWC,XNDC,YNDC)
*
* (C) COPYRIGHT SERC 1987
*
*-----------------------------------------------------------------------
*
*  RAL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             PJWR
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Transform from WC to NDC for output.
*
*  MAINTENANCE LOG
*  ---------------
*     21/09/87  PJWR  Created from GKTWN by AS as part of fix for bug
*                     S281.
*
*  ARGUMENTS
*  ---------
*     INP  IT      Transformation number
*     INP  N       Number of points
*     INP  XWC     World coordinate(s)
*     INP  YWC     World coordinate(s)
*     OUT  XNDC    Normalized device coordinate(s)
*     OUT  YNDC    Normalized device coordinate(s)
*
      INTEGER IT, N
      REAL XWC(N), YWC(N), XNDC(N), YNDC(N)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gksl.cmn'
*
*  LOCALS
*  ------
*     WNWD    Width of WC window in WC
*     WNHT    Height of WC window in WC
*     SCALEX  Scale relationship of WC to NDC in X
*     SCALEY  Scale relationship of WC to NDC in Y
*     SHIFTX  Shift relationship of WC to NDC in X
*     SHIFTY  Shift relationship of WC to NDC in Y
*     I       Loop variable
*
      REAL WNWD, WNHT, SCALEX, SCALEY, SHIFTX, SHIFTY
      INTEGER I
*
*-----------------------------------------------------------------------

*     Evaluate the width and height of the WC window
      WNWD = QLWXR(IT) - QLWXL(IT)
      WNHT = QLWYT(IT) - QLWYB(IT)
*     Evaluate the scale and shift terms used in the transformation
      SCALEX = (QLVPXR(IT) - QLVPXL(IT)) / WNWD
      SCALEY = (QLVPYT(IT) - QLVPYB(IT)) / WNHT
      SHIFTX = (QLVPXL(IT)*QLWXR(IT) - QLVPXR(IT)*QLWXL(IT)) / WNWD
      SHIFTY = (QLVPYB(IT)*QLWYT(IT) - QLVPYT(IT)*QLWYB(IT)) / WNHT
*     Transform WC to NDC
      DO 10 I=1,N
        XNDC(I) = XWC(I) * SCALEX + SHIFTX
        YNDC(I) = YWC(I) * SCALEY + SHIFTY
   10 CONTINUE

      END
