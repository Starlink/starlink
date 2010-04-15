*+BEARING Gets Bearing, +ve Clockwise, from point 1 to point 2
      SUBROUTINE BEARING(RA1,DEC1,RA2,DEC2,ROLL)

*  Type declarations
      IMPLICIT NONE

*  Calling Arguments
*     Input
      REAL RA1,DEC1	! Reference point coordinates, radian
      REAL RA2,DEC2	! Coordinates of point defining end of
					! rotation, radian
*     Output
      REAL ROLL		! Rotation angle, radian

*  Method
*     In the spherical triangle involving the two points and the North
*     pole, the angle at the ref. point is found. Checks are done to
*     find the correct quadrant, and to sort out special cases

*  History
*     1st version			M Ricketts	1987 Jan.
*     Modified to return roll +ve
*     Clockwise				M Denby		1988 Mar.
*-

*  Constant Parameters
      REAL PIBY2,PI,TWOPI
      PARAMETER(PIBY2=1.5707963,PI=3.14159265,TWOPI=6.2831853)

*  Local Variables
      REAL DELRA			! RA2 - RA1
      LOGICAL GTPI			! True if DELRA>pi, ie ROLL > pi
      REAL*8 POLE_TO_1, POLE_TO_2	! Angle, pole to each point
      REAL*8 DDELRA

*  Executable code

      DELRA = MOD(TWOPI + RA2 - RA1,TWOPI)
      IF (DELRA.LE.PI) THEN
         GTPI = .FALSE.
      ELSE
         GTPI = .TRUE.
         DELRA = TWOPI - DELRA
      END IF

      IF (DELRA.EQ.0.) THEN		! Spher. tri formula not useable
         IF (DEC1.LT.DEC2) THEN
            ROLL = 0.
         ELSE
            ROLL = PI
         END IF
      ELSE
         DDELRA = DELRA
         POLE_TO_1 = PIBY2 - DEC1
         POLE_TO_2 = PIBY2 - DEC2
         ROLL = ATAN( SIN(DDELRA) / ( (SIN(POLE_TO_1)/TAN(POLE_TO_2)) -
     &                                COS(DDELRA) * COS(POLE_TO_1) ) )
         IF (ROLL.LT.0.) ROLL = ROLL + PI
         IF (GTPI) ROLL = TWOPI - ROLL
      END IF

      ROLL = -ROLL

      END
