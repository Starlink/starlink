      SUBROUTINE sla_DS2C6 (A, B, R, AD, BD, RD, V)
*+
*     - - - - - -
*      D S 2 C 6
*     - - - - - -
*
*  Conversion of position & velocity in spherical coordinates
*  to Cartesian coordinates
*
*  (double precision)
*
*  Given:
*     A     dp      longitude (radians)
*     B     dp      latitude (radians)
*     R     dp      radial coordinate
*     AD    dp      longitude derivative (radians per unit time)
*     BD    dp      latitude derivative (radians per unit time)
*     RD    dp      radial derivative
*
*  Returned:
*     V     dp(6)   Cartesian position & velocity vector
*
*  P.T.Wallace   Starlink   10 July 1993
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      DOUBLE PRECISION A,B,R,AD,BD,RD,V(6)

      DOUBLE PRECISION SA,CA,SB,CB,RCB,X,Y,RBD,W



*  Useful functions
      SA=SIN(A)
      CA=COS(A)
      SB=SIN(B)
      CB=COS(B)
      RCB=R*CB
      X=RCB*CA
      Y=RCB*SA
      RBD=R*BD
      W=RBD*SB-CB*RD

*  Position
      V(1)=X
      V(2)=Y
      V(3)=R*SB

*  Velocity
      V(4)=-Y*AD-W*CA
      V(5)=X*AD-W*SA
      V(6)=RBD*CB+SB*RD

      END
