C# IL>=a, OL>=0
      SUBROUTINE GKRECT(XMIN,XMAX,YMIN,YMAX)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Front end
*  Author:             JRG
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To check that the 4 parameters represent a valid specification
*     of the limits of a rectangle in GKS. It is necessary to check
*     that for each of x and y, the min is less than the max and that
*     the extent is not too tiny. If the check fails, KERROR is set.
*
*  MAINTENANCE LOG
*  ---------------
*      29/6/83   JRG  Original version stabilized
*
*  ARGUMENTS
*  ---------
*     Inp   XMIN,XMAX  Limits in the x-direction
*     Inp   YMIN,YMAX  Limits in the y-direction
*
      REAL XMIN,XMAX,YMIN,YMAX
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify  /GKYERR/ Set KERROR if error
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkerr.cmn'
*
*  ERRORS
*  ------
*      51    Rectangle invalid
*
*---------------------------------------------------------------------


*   Use machine tolerance QTOL from PARAMETER definitions
      IF( XMAX-XMIN .LE. 10.0*QTOL*ABS(XMIN)  .OR.
     :    YMAX-YMIN .LE. 10.0*QTOL*ABS(YMIN)  )   KERROR=51
      END
