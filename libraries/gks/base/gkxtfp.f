C# IL>=a, OL>=0
      SUBROUTINE GKXTFP(XWDVEC,YWDVEC,XHTVEC,YHTVEC,XTRN,
     :                     YTRN,RX,RY)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             FY
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     transform text points
*
*  MAINTENANCE LOG
*  ---------------
*     10/03/83    FY  Original version stabilized
*     20/04/83    FY  split arrays with mixed x & y points
*
*  ARGUMENTS
*  ---------
*     INP XWDVEC width vector
*     INP YWDVEC
*     INP XHTVEC height vector
*     INP YHTVEC
*     INP XTRN   translation vector
*     INP YTRN
*     I/O RX     x component of point to be transformed
*     I/O RY     y component of point to be transformed
*
      REAL XWDVEC,YWDVEC,XHTVEC,YHTVEC,XTRN,YTRN,RX,RY
*
*  LOCALS
*  ------
*     TX     temp. variable
*     TY     temp. var.
*
      REAL TX,TY
*
*---------------------------------------------------------------------


      TX = RX*XWDVEC + RY*XHTVEC
      TY = RX*YWDVEC + RY*YHTVEC
      RX = TX + XTRN
      RY = TY + YTRN

      END
