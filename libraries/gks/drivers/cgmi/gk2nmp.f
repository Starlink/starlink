      SUBROUTINE GK2NMP(RLA)
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    (Part of) Workstation driver
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  This routine returns 3 points on the edge of
*  a circular arc after being given the centre
*  plus the start, end vectors & radius.
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver GK2NWD
*
*  ARGUMENTS
*  ---------
*   INP/OUT RLA    : Input/output array of points
*                 Input  RLA(1),RLA(2) Centre Point
*                        RLA(3),RLA(4) Centre to Start pt vector
*                        RLA(5),RLA(6) Centre to End pt vector
*                        RLA(7)        Radius
*                 Output RLA(1),RLA(2) Start Point
*                        RLA(3),RLA(4) Intermediate Point
*                        RLA(5),RLA(6) End Point
*
*  LOCALS
*  ------
*        I      : Loop counter
*        ANGLE  : Used for holding angles in radians
*                 ANGLE(1) Angle of start point vector
*                 ANGLE(2) Angle of end point vector
*                 ANGLE(3) Angle of intermediate point vector
*        XC,YC  : Centre point
*        TWOPI  : Constant with value of two pi


      INTEGER I
      REAL RLA(7), ANGLE(3), XC,YC, TWOPI

*---------------------------------------------------------------------

*     Set 2*PI
      TWOPI = 8.0*ATAN(1.0)

*   Get the angles between the X-axis and each of the two vectors
      ANGLE(1) = ATAN2 (RLA(4),RLA(3))
      ANGLE(3) = ATAN2 (RLA(6),RLA(5))

*   Arc runs from ANGLE(1) to ANGLE(3) in direction of increasing angle.
*    To select mid point vector angle ANGLE(3) must be at least ANGLE(1).
      IF(ANGLE(3) .LT. ANGLE(1))THEN
         ANGLE(3) = ANGLE(3) + TWOPI
      ENDIF

*   Calculate the angle of the mid point from the X-axis
      ANGLE(2)=(ANGLE(3)+ANGLE(1))/2.0

*   Put all angles in the range (0:two pi)
      DO 190 I=1,3
         IF(ANGLE(I) .LT. 0.0)THEN
           ANGLE(I) = ANGLE(I) + TWOPI
         ELSEIF(ANGLE(I) .GE. TWOPI)THEN
           ANGLE(I) = ANGLE(I) - TWOPI
         ENDIF
  190 CONTINUE
      XC = RLA(1)
      YC = RLA(2)

*   Work out the coordinates of the 3 new points on circumference
      RLA(1) = XC + RLA(7)*(COS(ANGLE(1)))
      RLA(2) = YC + RLA(7)*(SIN(ANGLE(1)))
      RLA(3) = XC + RLA(7)*(COS(ANGLE(2)))
      RLA(4) = YC + RLA(7)*(SIN(ANGLE(2)))
      RLA(5) = XC + RLA(7)*(COS(ANGLE(3)))
      RLA(6) = YC + RLA(7)*(SIN(ANGLE(3)))

      RETURN
      END
