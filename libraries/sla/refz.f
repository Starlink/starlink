      SUBROUTINE sla_REFZ (ZU, REFA, REFB, ZR)
*+
*     - - - - -
*      R E F Z
*     - - - - -
*
*  Adjust an unrefracted zenith distance to include the effect of
*  atmospheric refraction, using the simple A tan Z + B tan**3 Z
*  model (plus special handling for large ZDs).
*
*  Given:
*    ZU    dp    unrefracted zenith distance of the source (radian)
*    REFA  dp    tan Z coefficient (radian)
*    REFB  dp    tan**3 Z coefficient (radian)
*
*  Returned:
*    ZR    dp    refracted zenith distance (radian)
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
*     refraction models;  the formula used here is based on the
*     Newton-Raphson method.  For the utmost numerical consistency
*     with the refracted to unrefracted model, two iterations are
*     carried out, achieving agreement at the 1D-11 arcseconds level
*     for a ZD of 80 degrees.  The inherent accuracy of the model
*     is, of course, far worse than this - see the documentation for
*     sla_REFCO for more information.
*
*  2  At ZD 83 degrees, the rapidly-worsening A tan Z + B tan**3 Z
*     model is abandoned and an empirical formula takes over.  Over a
*     wide range of observer heights and corresponding temperatures and
*     pressures, the following levels of accuracy (arcsec) are
*     typically achieved, relative to numerical integration through a
*     model atmosphere:
*
*              ZR    error
*
*              80      0.4
*              81      0.8
*              82      1.5
*              83      3.2
*              84      4.9
*              85      5.8
*              86      6.1
*              87      7.1
*              88     10
*              89     20
*              90     40
*              91    100         } relevant only to
*              92    200         } high-elevation sites
*
*     The high-ZD model is scaled to match the normal model at the
*     transition point;  there is no glitch.
*
*  3  Beyond 93 deg zenith distance, the refraction is held at its
*     93 deg value.
*
*  4  See also the routine sla_REFV, which performs the adjustment in
*     Cartesian Az/El coordinates, and with the emphasis on speed
*     rather than numerical accuracy.
*
*  P.T.Wallace   Starlink   19 September 1995
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      DOUBLE PRECISION ZU,REFA,REFB,ZR

*  Radians to degrees
      DOUBLE PRECISION R2D
      PARAMETER (R2D=57.29577951308232D0)

*  Largest usable ZD (deg)
      DOUBLE PRECISION D93
      PARAMETER (D93=93D0)

*  Coefficients for high ZD model (used beyond ZD 83 deg)
      DOUBLE PRECISION C1,C2,C3,C4,C5
      PARAMETER (C1=+0.55445D0,
     :           C2=-0.01133D0,
     :           C3=+0.00202D0,
     :           C4=+0.28385D0,
     :           C5=+0.02390D0)

*  ZD at which one model hands over to the other (radians)
      DOUBLE PRECISION Z83
      PARAMETER (Z83=83D0/R2D)

*  High-ZD-model prediction (deg) for that point
      DOUBLE PRECISION REF83
      PARAMETER (REF83=(C1+C2*7D0+C3*49D0)/(1D0+C4*7D0+C5*49D0))

      DOUBLE PRECISION ZU1,ZL,S,C,T,TSQ,TCU,REF,E,E2



*  Perform calculations for ZU or 83 deg, whichever is smaller
      ZU1 = MIN(ZU,Z83)

*  Functions of ZD
      ZL = ZU1
      S = SIN(ZL)
      C = COS(ZL)
      T = S/C
      TSQ = T*T
      TCU = T*TSQ

*  Refracted ZD (mathematically to better than 1 mas at 70 deg)
      ZL = ZL-(REFA*T+REFB*TCU)/(1D0+(REFA+3D0*REFB*TSQ)/(C*C))

*  Further iteration
      S = SIN(ZL)
      C = COS(ZL)
      T = S/C
      TSQ = T*T
      TCU = T*TSQ
      REF = ZU1-ZL+
     :          (ZL-ZU1+REFA*T+REFB*TCU)/(1D0+(REFA+3D0*REFB*TSQ)/(C*C))

*  Special handling for large ZU
      IF (ZU.GT.ZU1) THEN
         E = 90D0-MIN(D93,ZU*R2D)
         E2 = E*E
         REF = (REF/REF83)*(C1+C2*E+C3*E2)/(1D0+C4*E+C5*E2)
      END IF

*  Return refracted ZD
      ZR = ZU-REF

      END
