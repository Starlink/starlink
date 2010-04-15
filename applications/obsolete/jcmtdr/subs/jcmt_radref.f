      SUBROUTINE JCMT_RADREFR (HT, TDK, PMB, RH, WL, LAT, A, B )

*+
*  Name:
*     JCMT_RADREFR

*  Purpose:
*     Supply the refraction coefficients for mm wave case

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL JCMT_RADREFR (HT, TDK, PMB, RH, WL, LAT, A, B )

*  Description:
*     Supplies the refraction coefficients A and B for the refraction
*     formula dz = Atan(z) + B tan(z)**3. At the moment it just uses
*     appropriate SLA routine. Commented out in the code is an earlier
*     version, written before the SLA routine handled the radio case,
*     which calculated A using the Radio case from Allen. "Astrophysical
*     Quantities" p. 124 and did not supply a value for B. The formula for
*     the water vapour pressure was obtained from "Vectorial Astrometry"
*     C.A. Murray

*  Arguments:
*     HT = DOUBLE PRECISION (Given)
*        The height of the observer above sea level (metres)
*     TDK = DOUBLE PRECISION (Given)
*        The ambient temperature DegK
*     PMB = DOUBLE PRECISION (Given)
*        Ambient pressure mbar
*     RH = DOUBLE PRECISION (Given)
*        Relative Humidity
*     WL = DOUBLE PRECISION (Given)
*        Wavelength (microns)
*     LAT = DOUBLE PRECISION (Given)
*        Latitude of the observer (radian)
*     A = DOUBLE PRECISION (Returned)
*        Refraction constant 1
*     B = DOUBLE PRECISION (Returned)
*        Refraction constant 2

*  [optional_subroutine_items]...
*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-JAN-1990 (JBVAD::PAH):
*        Original version.
*     13-MAY-1991 (REVAD::JFL): Modified to use SLA_REFCO
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      DOUBLE PRECISION HT, TDK, PMB, RH, WL, LAT

*  Arguments Returned:
      DOUBLE PRECISION A, B

*  Local Constants:
      DOUBLE PRECISION MBAR2MMHG ! Conversion factor from mBar to mmHg
      PARAMETER ( MBAR2MMHG = 1.333224d0 )
      DOUBLE PRECISION MWTH20    ! Molecular weight of water
      PARAMETER ( MWTH20 = 18 )
      DOUBLE PRECISION RGAS      ! universal gas constant
      PARAMETER ( RGAS = 8.31D0 )
      DOUBLE PRECISION TLR       ! temperature lapse rate in the troposphere
      PARAMETER ( TLR = 0.0065D0 ) ! (K per metre)
      DOUBLE PRECISION EPS       ! precision required to terminate SLA_REFCO
      PARAMETER ( EPS = 1D-10 )  ! iteration

*  Local Variables:
      DOUBLE PRECISION N         ! Refractve index of air
      DOUBLE PRECISION WVP       ! Water vapour pressure in mmHg
      DOUBLE PRECISION SATDEN    ! saturated water vapour density
      DOUBLE PRECISION PSAT      ! saturated water vapour pressure
      DOUBLE PRECISION TC        ! temperature in centigrate

*.

****************************************************************************
*  THIS IS THE ORIGINAL VERSION WHICH USED THE RADIO CASE FROM A.Q.
*
*  convert wavelength to metres
*
*      WML = WL / 1.0D6
*
*  temperature in centigrade
*
*      TC = TDK-273.15
*
*  calculate the saturated water vapour density at this temperature
*
*      SATDEN = EXP (-5.32917 + 0.0688825*TC
*     :              -2.9815D-4*TC**2
*     :              + 1.39D-6*TC**3)
*
*  Calculate saturated water vapour pressure
*
*      PSAT = SATDEN * RGAS * TDK / MWTH20
*
*  Calculate water vapour pressure
*
*      WVP = RH * PSAT * MBAR2MMHG
*
*  Calculate the refractive index
*
*      N = (287.8 * PMB * MBAR2MMHG / 760d0 / (1+0.00366 * TC)
*     :        + 0.33*WVP/(1+0.00366 * TC)
*     :        + 6.7*WVP/(1+0.00366 * TC)**2)/1D6+1
*
*  first refraction constant
*
*      A = (N**2-1)/2/N**2
*
*  second refraction constant (set to zero at the moment)
*
*      B = 0
*
*******************************************************************************

*  call the SLA routine

      CALL SLA_REFCO (HT, TDK, PMB, RH, WL, LAT, TLR, EPS, A, B)

      END
