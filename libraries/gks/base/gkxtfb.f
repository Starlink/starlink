C# IL>=a, OL>=0
      SUBROUTINE GKXTFB(XWDVEC,YWDVEC,XHTVEC,YHTVEC,XTRN,YTRN,
     :                     XTX,YTX)
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
*     transform coordinates of a box
*
*  MAINTENANCE LOG
*  ---------------
*     10/03/83    FY  Original version stabilized
*     20/04/84    FY  split arrays with mixed x & y points
*
*  ARGUMENTS
*  ---------
*     INP XWDVEC width vector
*     INP YWDVEC
*     INP XHTVEC height vector
*     INP YHTVEC
*     INP XTRN   translation vector
*     INP YTRN
*     I/O XTX    text extent
*     I/O YTX
*
      REAL XWDVEC,YWDVEC,XHTVEC,YHTVEC,XTRN,YTRN,XTX(4),YTX(4)
*
*  LOCALS
*  ------
*     T1     temp. var. for intermediate value
*     T2     temp. var. for intermediate value
*
      INTEGER I
      REAL T1,T2
*
*---------------------------------------------------------------------


      DO 10 I=1,4
        T1 = XTX(I)*XWDVEC + YTX(I)*XHTVEC + XTRN
        T2 = XTX(I)*YWDVEC + YTX(I)*YHTVEC + YTRN
        XTX(I) = T1
        YTX(I) = T2
   10 CONTINUE
      END
