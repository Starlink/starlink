      SUBROUTINE sla_DCS2C (A, B, V)
*+
*     - - - - - -
*      D C S 2 C
*     - - - - - -
*
*  Spherical coordinates to direction cosines (double precision)
*
*  Given:
*     A,B       dp      spherical coordinates in radians
*                        (RA,Dec), (Long,Lat) etc
*
*  Returned:
*     V         dp(3)   x,y,z unit vector
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

      DOUBLE PRECISION A,B,V(3)

      DOUBLE PRECISION COSB



      COSB=COS(B)

      V(1)=COS(A)*COSB
      V(2)=SIN(A)*COSB
      V(3)=SIN(B)

      END
