      REAL FUNCTION sla_RVGALC (R2000, D2000)
*+
*     - - - - - - -
*      R V G A L C
*     - - - - - - -
*
*  Velocity component in a given direction due to the rotation
*  of the Galaxy (single precision)
*
*  Given:
*     R2000,D2000   real    J2000.0 mean RA,Dec (radians)
*
*  Result:
*     Component of dynamical LSR motion in direction R2000,D2000 (km/s)
*
*  Sign convention:
*     The result is +ve when the dynamical LSR is receding from the
*     given point on the sky.
*
*  Note:  The Local Standard of Rest used here is a point in the
*         vicinity of the Sun which is in a circular orbit around
*         the Galactic centre.  Sometimes called the "dynamical" LSR,
*         it is not to be confused with a "kinematical" LSR, which
*         is the mean standard of rest of star catalogues or stellar
*         populations.
*
*  Reference:  The orbital speed of 220 km/s used here comes from
*              Kerr & Lynden-Bell (1986), MNRAS, 221, p1023.
*
*  Called:
*     sla_CS2C, sla_VDV
*
*  P.T.Wallace   Starlink   23 March 1994
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      REAL R2000,D2000

      REAL VA(3), VB(3)

      REAL sla_VDV

*
*  LSR velocity due to Galactic rotation
*
*  Speed = 220 km/s
*  Apex  = L2,B2  90deg, 0deg
*        = RA,Dec  21 12 01.1  +48 19 47  J2000.0
*
*  This is expressed in the form of a J2000.0 x,y,z vector:
*
*      VA(1) = X = -SPEED*COS(RA)*COS(DEC)
*      VA(2) = Y = -SPEED*SIN(RA)*COS(DEC)
*      VA(3) = Z = -SPEED*SIN(DEC)

      DATA VA / -108.70408, +97.86251, -164.33610 /



*  Convert given J2000 RA,Dec to x,y,z
      CALL sla_CS2C(R2000,D2000,VB)

*  Compute dot product with LSR motion vector
      sla_RVGALC=sla_VDV(VA,VB)

      END
