      REAL FUNCTION sla_RVLG (R2000, D2000)
*+
*     - - - - -
*      R V L G
*     - - - - -
*
*  Velocity component in a given direction due to the combination
*  of the rotation of the Galaxy and the motion of the Galaxy
*  relative to the mean motion of the local group (single precision)
*
*  Given:
*     R2000,D2000   real    J2000.0 mean RA,Dec (radians)
*
*  Result:
*     Component of SOLAR motion in direction R2000,D2000 (km/s)
*
*  Sign convention:
*     The result is +ve when the Sun is receding from the
*     given point on the sky.
*
*  Reference:
*     IAU Trans 1976, 168, p201.
*
*  Called:
*     sla_CS2C, sla_VDV
*
*  P.T.Wallace   Starlink   June 1985
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      REAL R2000,D2000

      REAL VA(3), VB(3)

      REAL sla_VDV

*
*  Solar velocity due to Galactic rotation and translation
*
*  Speed = 300 km/s
*
*  Apex  = L2,B2  90deg, 0deg
*        = RA,Dec  21 12 01.1  +48 19 47  J2000.0
*
*  This is expressed in the form of a J2000.0 x,y,z vector:
*
*      VA(1) = X = -SPEED*COS(RA)*COS(DEC)
*      VA(2) = Y = -SPEED*SIN(RA)*COS(DEC)
*      VA(3) = Z = -SPEED*SIN(DEC)

      DATA VA / -148.23284, +133.44888, -224.09467 /



*  Convert given J2000 RA,Dec to x,y,z
      CALL sla_CS2C(R2000,D2000,VB)

*  Compute dot product with Solar motion vector
      sla_RVLG=sla_VDV(VA,VB)

      END
