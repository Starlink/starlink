C# IL>=a, OL>=0
      SUBROUTINE GKMTDV(XIN,YIN,XFORM)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  Utility to derive matrix
*  Author:             JGW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Given three points [1,0] [0,1] [1,1] are assumed to have been
*     transformed. This utility will identify and return xform matrix.
*
*  MAINTENANCE LOG
*  ---------------
*     19/12/83 JGW  Original version stabilized
*     20/12/83 AS   Tidy up
*
*  ARGUMENTS
*  ---------
*     INP   XIN     Three points.
*     INP   YIN     Three points.
*     OUT   XFORM   Transform matrix.
*
      REAL  XIN(*),YIN(*),XFORM(3,2)
*
*---------------------------------------------------------------------

      XFORM(1,1) = XIN(3) - XIN(2)
      XFORM(2,1) = XIN(3) - XIN(1)
      XFORM(3,1) = XIN(1) + XIN(2) - XIN(3)
      XFORM(1,2) = YIN(3) - YIN(2)
      XFORM(2,2) = YIN(3) - YIN(1)
      XFORM(3,2) = YIN(1) + YIN(2) - YIN(3)
      END
