
C  IL>=a, OL>=0
      SUBROUTINE GKTNWV(XNDC,YNDC,XWC,YWC)
*
* (C) COPYRIGHT ICL & SERC  1987
*
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Utility
*  Author:             DCS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Transform vector from NDC to WC using current normalization
*     transformation.
*
*  MAINTENANCE LOG
*  ---------------
*     20/01/87  DCS   IS conversion. Original version stabilized.
*     09/03/87  DCS   Removed comment from INCLUDE GKSL.CMN.
*                     Add INCLUDE GKDT.PAR.
*     23/03/87  RMK   Moved INCLUDEs to start in column 7 (S247).
*
*  ARGUMENTS
*  ---------
*     INP    XNDC   Normalized device coordinate vector X
*     INP    YNDC   Normalized device coordinate vector Y
*     OUT    XWC    World coordinate vector X
*     OUT    YWC    World coordinate vector Y
*
      REAL XNDC,YNDC,XWC,YWC
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKYSL/  Current NT, Window and Viewport
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gksl.cmn'
*
*  ERRORS
*  ------
*     None
*
*---------------------------------------------------------------------
      XWC = XNDC * (QLWXR(KCNTN) - QLWXL(KCNTN)) /
     :             (QLVPXR(KCNTN) - QLVPXL(KCNTN))
      YWC = YNDC * (QLWYT(KCNTN) - QLWYB(KCNTN)) /
     :             (QLVPYT(KCNTN) - QLVPYB(KCNTN))
      END
