*+RS_A2V         Converts azimuth, elevation ( radian) to unit vector      VX
      SUBROUTINE RS_A2V(AZ,EL,V)
 
*  Calling Arguments
      REAL AZ,EL	! In	Direction, Spherical coordinates
      REAL V(3)		! Out	Unit vector
*  Author:		M Ricketts	RAL	1988 improved version
*  June 1992		M. Duesterhaus	Remove VAX RTL calls
************************************************************************
*-
 
*  Local Variables
      REAL SINAZ, COSAZ, COSEL
 
      SINAZ = SIN(AZ)
      COSAZ = COS(AZ)
      V(3) = SIN(EL)
      COSEL = COS(EL)
      V(1) = COSAZ * COSEL
      V(2) = SINAZ * COSEL
 
      END
