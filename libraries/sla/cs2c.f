      SUBROUTINE sla_CS2C (A, B, V)
*+
*     - - - - -
*      C S 2 C
*     - - - - -
*
*  Spherical coordinates to direction cosines (single precision)
*
*  Given:
*     A,B      real      spherical coordinates in radians
*                        (RA,Dec), (Long,Lat) etc
*
*  Returned:
*     V        real(3)   x,y,z unit vector
*
*  The spherical coordinates are longitude (+ve anticlockwise
*  looking from the +ve latitude pole) and latitude.  The
*  Cartesian coordinates are right handed, with the x axis
*  at zero longitude and latitude, and the z axis at the
*  +ve latitude pole.
*
*  P.T.Wallace   Starlink   October 1984
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      REAL A,B,V(3)

      REAL COSB



      COSB=COS(B)

      V(1)=COS(A)*COSB
      V(2)=SIN(A)*COSB
      V(3)=SIN(B)

      END
