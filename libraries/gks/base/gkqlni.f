C# IL>=a, OL>=0
      SUBROUTINE GKQLNI (XL1,YL1,XL2,YL2,CROSS,COLLIN,XC,YC)
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
*     Determine whether two line segments intersect and if so
*     give the intersection point.
*
*  MAINTENANCE LOG
*  ---------------
*     20/04/89  KEVP  Eliminated false positive in event of 1st line
*                     being of negligible length.
*     25/04/89  KEVP  Removed debug WRITE statements.
*     22/07/90  PLP   Commenting brought in line with standard format.
*     29/10/91  KEVP  Improved precision (C69).
*
*  ARGUMENTS
*  ---------
*     INP  XL1,YL1  The end points of the first line segment
*     INP  XL2,YL2  The end points of the second line segment
*     OUT  CROSS    True, if the lines intersect
*     OUT  COLLIN   True, if CROSS and lines are collinear
*     OUT  XC,YC    The intersection point, if CROSS is true
*                      If the lines are colinear, both end pts of
*                      the intersection are given.
*
      REAL     XL1(2),YL1(2), XL2(2),YL2(2), XC(2),YC(2)
      LOGICAL  CROSS, COLLIN
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkmc.par'
*
*  LOCALS
*  ------
*     IM1,IM2  Indices of minimum values in line arrays
*     IC       Index for end-points of collinear intersection
*     AX,AY    Vector from start of 1st line to start of 2nd line
*     BX,BY    Vector from end   of 1st line to start of 2nd line
*     DX,DY    Vector from start of 1st line to end   of 2nd line
*     EX,EY    Vector from end   of 1st line to end   of 2nd line
*     SIDE1    Cross product for first side of 1st line
*     SIDE2    Cross product for second side of 1st line
*     SIDE3    Cross product for first side of 2nd line
*     SIDE4    Cross product for second side of 2nd line
*     SQL1     Squared length of 1st line
*     TINY     Tolerance on SIDE1-SIDE2 and SQL1
*     UX,UY    Vector from start to end of 1st line
*     WGT      Weighting factor used to determine crossing point
*
      INTEGER   IM1,IM2, IC
      REAL      AX,AY, BX,BY, DX,DY, EX,EY, UX,UY
      REAL*8    SIDE1,SIDE2, SIDE3,SIDE4, TINY, WGT, SQL1
*
*  ALGORITHM
*  ---------
*     Variables in distance squared are double precision.
*
*     NB: Need to deal with co-linearity ie when SIDE1 = SIDE2 = 0.0
*
*---------------------------------------------------------------------
*
*  Initialise at NO CROSSING
      CROSS = .FALSE.
      COLLIN = .FALSE.
*
*  Do the bounding boxes intersect?
      IM1 = 1
      IF(YL1(1) .GT. YL1(2)) IM1 = 2
      IM2 = 1
      IF(YL2(1) .GT. YL2(2)) IM2 = 2
      IF(YL1(IM1) .GT. YL2(3-IM2))GOTO 999
      IF(YL2(IM2) .GT. YL1(3-IM1))GOTO 999
      IM1 = 1
      IF(XL1(1) .GT. XL1(2)) IM1 = 2
      IM2 = 1
      IF(XL2(1) .GT. XL2(2)) IM2 = 2
      IF(XL1(IM1) .GT. XL2(3-IM2))GOTO 999
      IF(XL2(IM2) .GT. XL1(3-IM1))GOTO 999

*  Here bounding boxes DO intersect
*     Do the two endpoints of the 2nd line occur on the same side
*     of the first line?
      AX = XL2(1) - XL1(1)
      AY = YL2(1) - YL1(1)
      BX = XL2(1) - XL1(2)
      BY = YL2(1) - YL1(2)
      SIDE1 = AX*BY - BX*AY
      DX = XL2(2) - XL1(1)
      DY = YL2(2) - YL1(1)
      EX = XL2(2) - XL1(2)
      EY = YL2(2) - YL1(2)
      SIDE2 = DX*EY - EX*DY
      IF((SIDE1 .GT. 0.0) .AND. (SIDE2 .GT. 0.0)) GOTO 999
      IF((SIDE1 .LT. 0.0) .AND. (SIDE2 .LT. 0.0)) GOTO 999
*
*  Check for collinearity
      TINY=QTOL*(AX*AX+AY*AY +BX*BX+BY*BY +DX*DX+DY*DY +EX*EX+EY*EY)
      IF( ABS(SIDE1 - SIDE2) .LT. TINY) THEN
*       First, check length of first line - might not be collinear
*                                           if this is negligible.
        UX = XL1(2) - XL1(1)
        UY = YL1(2) - YL1(1)
        SQL1 = UX*UX + UY*UY
        IF(ABS(SQL1) .LT. TINY) THEN
          SIDE3 = AX*DY - DX*AY
*         If one end of the 1st line is away from the 2nd line,
*         there is no intersection.
          IF(ABS(SIDE3) .GT. TINY)GOTO 999
        ENDIF
*       Here lines are collinear and
        COLLIN = .TRUE.
*       since bounding boxes intersect, the lines must intersect.
        CROSS = .TRUE.
*       Find end points of intersection and quit
        IC = 1
        IF((AX*DX + AY*DY) .LE. 0.0)THEN
            XC(1) = XL1(1)
            YC(1) = YL1(1)
            IC = 2
        ENDIF
        IF((AX*BX + AY*BY) .LE. 0.0)THEN
            XC(IC) = XL2(1)
            YC(IC) = YL2(1)
            IC = IC+1
        ENDIF
        IF(IC .GT. 2)GOTO 999
        IF((BX*EX + BY*EY) .LE. 0.0)THEN
            XC(IC) = XL1(2)
            YC(IC) = YL1(2)
            IC = IC+1
        ENDIF
        IF(IC .GT. 2)GOTO 999
        IF((DX*EX + DY*EY) .LE. 0.0)THEN
            XC(2) = XL2(2)
            YC(2) = YL2(2)
        ENDIF
        GOTO 999
      ENDIF

*  Here the end points of the 2nd line occur at different sides of
*  the first line.
*     Is also true for the end points of the 1st w.r.t. the 2nd.
      SIDE3 = AX*DY - DX*AY
      SIDE4 = BX*EY - EX*BY
      IF((SIDE3 .GT. 0.0) .AND. (SIDE4 .GT. 0.0)) GOTO 999
      IF((SIDE3 .LT. 0.0) .AND. (SIDE4 .LT. 0.0)) GOTO 999

*  Here the lines must cross
      CROSS = .TRUE.

*  Find Crossing point
      WGT = SIDE1/(SIDE1-SIDE2)
      XC(1) = XL2(1) + WGT*(XL2(2)-XL2(1))
      YC(1) = YL2(1) + WGT*(YL2(2)-YL2(1))
      XC(2) = XC(1)
      YC(2) = YC(1)

  999 CONTINUE

      END
