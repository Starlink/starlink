C# IL>=b, OL>=0
      SUBROUTINE GKTWNV(IT,XWC,YWC,XNDC,YNDC)
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
*     Transform vector from WC to NDC
*
*  MAINTENANCE LOG
*  ---------------
*     02/12/83  AS    Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP  IT      Transformation number
*     INP  XWC     World coordinate
*     INP  YWC     World coordinate
*     OUT  XNDC    Normalized device coordinate
*     OUT  YNDC    Normalized device coordinate
*
      INTEGER IT
      REAL XWC, YWC, XNDC, YNDC
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*---------------------------------------------------------------------



* Transform WC to NDC

        XNDC = XWC*(QLVPXR(IT) - QLVPXL(IT))/(QLWXR(IT)-QLWXL(IT))
        YNDC = YWC*(QLVPYT(IT) - QLVPYB(IT))/(QLWYT(IT)-QLWYB(IT))

      END
