      SUBROUTINE sla_REFV (VU, REFA, REFB, VR)
*+
*     - - - - -
*      R E F V
*     - - - - -
*
*  Adjust an unrefracted Cartesian vector to include the effect of
*  atmospheric refraction, using the simple A tan Z + B tan**3 Z
*  model.
*
*  Given:
*    VU    dp    unrefracted position of the source (Az/El 3-vector)
*    REFA  dp    tan Z coefficient (radian)
*    REFB  dp    tan**3 Z coefficient (radian)
*
*  Returned:
*    VR    dp    refracted position of the source (Az/El 3-vector)
*
*  Notes:
*
*  1  This routine applies the adjustment for refraction in the
*     opposite sense to the usual one - it takes an unrefracted
*     (in vacuo) position and produces an observed (refracted)
*     position, whereas the A tan Z + B tan**3 Z model strictly
*     applies to the case where an observed position is to have the
*     refraction removed.  The unrefracted to refracted case is
*     harder, and requires an inverted form of the text-book
*     refraction models;  the algorithm used here is equivalent to
*     one iteration of the Newton-Raphson method applied to the above
*     formula.
*
*  2  Though optimized for speed rather than precision, the present
*     routine achieves consistency with the refracted-to-unrefracted
*     A tan Z + B tan**3 Z model at better than 1 microarcsecond within
*     30 degrees of the zenith and remains within 1 milliarcsecond to
*     beyond ZD 70 degrees.  The inherent accuracy of the model is, of
*     course, far worse than this - see the documentation for sla_REFCO
*     for more information.
*
*  3  At low elevations (below about 3 degrees) the refraction
*     correction is held back to prevent arithmetic problems and
*     wildly wrong results.  Over a wide range of observer heights
*     and corresponding temperatures and pressures, the following
*     levels of accuracy (arcsec) are achieved, relative to numerical
*     integration through a model atmosphere:
*
*              ZD    error
*
*              80      0.4
*              81      0.8
*              82      1.6
*              83      3
*              84      7
*              85     17
*              86     45
*              87    150
*              88    340
*              89    620
*              90   1100
*              91   1900         } relevant only to
*              92   3200         } high-elevation sites
*
*  4  See also the routine sla_REFZ, which performs the adjustment to
*     the zenith distance rather than in Cartesian Az/El coordinates.
*     The present routine is faster than sla_REFZ and, except very low down,
*     is equally accurate for all practical purposes.  However, beyond
*     about ZD 84 degrees sla_REFZ should be used, and for the utmost
*     accuracy iterative use of sla_REFRO should be considered.
*
*  P.T.Wallace   Starlink   26 December 1994
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      DOUBLE PRECISION VU(3),REFA,REFB,VR(3)

      DOUBLE PRECISION X,Y,Z1,Z,ZSQ,RSQ,R,WB,WT,D,CD,F



*  Initial estimate = unrefracted vector
      X = VU(1)
      Y = VU(2)
      Z1 = VU(3)

*  Keep correction approximately constant below about 3 deg elevation
      Z = MAX(Z1,0.05D0)

*  One Newton-Raphson iteration
      ZSQ = Z*Z
      RSQ = X*X+Y*Y
      R = SQRT(RSQ)
      WB = REFB*RSQ/ZSQ
      WT = (REFA+WB)/(1D0+(REFA+3D0*WB)*(ZSQ+RSQ)/ZSQ)
      D = WT*R/Z
      CD = 1D0-D*D/2D0
      F = CD*(1D0-WT)

*  Post-refraction x,y,z
      VR(1) = X*F
      VR(2) = Y*F
      VR(3) = CD*(Z+D*R)+(Z1-Z)

      END
