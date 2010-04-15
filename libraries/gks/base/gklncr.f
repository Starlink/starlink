C# IL>=a, OL>=0
      SUBROUTINE GKLNCR (RECT,XL1,YL1,XL2,YL2,CROSS)
*
* (C) COPYRIGHT ICL & SERC  1989
*

*-------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             KEVP
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To determine whether a line cross a rectangle
*
*  MAINTENANCE LOG
*  ---------------
*     26/04/89  KEVP  Original Version Stabilised.
*     22/07/90  PLP   Commenting brought in line with standard format.
*
*  ARGUMENTS
*  ---------
*     INP  RECT    The rectangle in form (xmin,xmax,ymin,ymax)
*     INP  XL1,YL1 First point defining the line
*     INP  XL2,YL2 Second point defining the line
*     OUT  CROSS   True, if they cross
*
*  Note:  XLn,YLn define a whole line, not a line segment.

      REAL    RECT(4), XL1,YL1, XL2,YL2
      LOGICAL CROSS
*
*  LOCALS
*  ------
*     XC,YC   The current corner
*     XCA,YCA Vector from first point of line to current corner
*     XCB,YCB Vector from second point of line to current corner
*
*     PROD    The cross product
*     ISIGN   The sign of the previous non-negligible cross product
*             zero if not any.
*     AREA    Area of rectangle
*     TINY    A very small constant
*
*     J       Corner number
*     JX      Index in RECT for X-Coordinate of corner
*     JY      Index in RECT for Y-Coordinate of corner
*
      INTEGER ISIGN, J, JX, JY
      REAL    XC,YC, XCA,YCA, XCB,YCB, PROD, AREA, TINY
      PARAMETER (TINY=1.0E-10)
*
*  ALGORITHM
*  ---------
*     Find the cross product between the vectors from each of
*    the two points of the line and a corner of the rectangle.
*
*    Repeat with each corner of rectangle until ether
*       (1) there is a change in sign in this cross product
*           in which case there is a crossing
*    or (2) all four corners have been done and
*           if all four cross products are of the same sign
*           there is no crossing
*
*    A cross product of zero is considered to have the same sign
*    as either a positive or negative cross product. Hence a tangent
*    line is not considered to cross the rectangle.
*
*---------------------------------------------------------------------
*
      ISIGN = 0
      AREA = (RECT(2)-RECT(1))*(RECT(4)-RECT(3))
      DO 10 J=0,3
        JX = 1 + J/2
        JY = 3 + J - 2*(J/2)
        XC = RECT(JX)
        YC = RECT(JY)
        XCA = XC-XL1
        XCB = XC-XL2
        YCA = YC-YL1
        YCB = YC-YL2
        PROD = (XCA*YCB - YCA*XCB)
        IF(ABS(PROD) .GT. TINY*AREA)THEN
           IF(PROD .GT. 0)THEN
              IF(ISIGN .LT. 0)THEN
                 CROSS = .TRUE.
                 GOTO 999
              ELSE
                 ISIGN = 1
              ENDIF
           ELSE
              IF(ISIGN .GT. 0)THEN
                 CROSS = .TRUE.
                 GOTO 999
              ELSE
                 ISIGN = -1
              ENDIF
           ENDIF
        ENDIF
   10 CONTINUE
      CROSS = .FALSE.
  999 CONTINUE
      END
