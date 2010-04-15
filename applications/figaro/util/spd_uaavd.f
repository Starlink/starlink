      DOUBLE PRECISION FUNCTION SPD_UAAVD(
     :   WRT, THETA, ALPHA, LGTC, LGNU )
*+
*  Name:
*     SPD_UAAV{DR}

*  Purpose:
*     Get one value for diluted Planck derivative.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = SPD_UAAVD( WRT, THETA, ALPHA, LGTC, LGNU )

*  Description:
*     This function returns for one given abscissa value the value
*     of the derivative of a diluted Planck profile with respect to one
*     of the profile parameters. Note that this is not the derivative
*     with respect to abscissa values.
*
*     The profile function is
*
*        f(lg(nu)) = Theta + alpha * lg(nu)
*                  + lg[ 2h/c^2 * nu^3 / [ exp(hnu/kT) - 1 ] ]
*
*     The derivatives are
*
*        df/dTheta = 1
*
*        df/dalpha = lg(nu)
*
*        df/dlg(T) = 10^lg(hnu/kT) / [ 1 - exp(-10^lg(hnu/kT)) ]
*
*     Whence lg(hnu/kT) falls outside a certain range the Wien or
*     Rayleigh-Jeans approximations are used for df/dlg(T):
*
*        df/dlg(T) ~= 1               R.-J.;  lg(hnu/kT) < -17
*
*        df/dlg(T) ~= 10^lg(hnu/kT)   Wien;   lg(hnu/kT) > 1.59106

*  Arguments:
*     WRT = INTEGER (Given)
*        1/2/3 in order to return the derivative w.r.t. Theta, alpha or
*        lg(T). If WRT has an invalid value, this routine will return a
*        result of zero.
*     THETA = DOUBLE PRECISION (Given)
*        The scaling constant of the profile.
*     ALPHA = DOUBLE PRECISION (Given)
*        The emissivity exponent of the profile.
*     LGTC = DOUBLE PRECISION (Given)
*        The logarithm of the colour temperature in Kelvin of the
*        profile.
*     LGNU = DOUBLE PRECISION (Given)
*        The abscissa value for which the profile derivative's ordinate
*        value is to be returned. This has to be the logarithm of the
*        frequency in Hertz.

*  Returned Value:
*     SPD_UAAVD = DOUBLE PRECISION
*        The ordinate value of the profile derivative at the given
*        abscissa value.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     26 Jan 1993 (hme):
*        Adapted from SPABAD.
*     19 Dec 1994 (hme):
*        Renamed from SPACWD.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER WRT
      DOUBLE PRECISION THETA
      DOUBLE PRECISION ALPHA
      DOUBLE PRECISION LGTC
      DOUBLE PRECISION LGNU

*  Local Constants:
      DOUBLE PRECISION LGHOVK    ! lg[(h/k)/(K/Hz)]
      PARAMETER ( LGHOVK = -10.318810511545D0 )

*  Local Variables:
      DOUBLE PRECISION LGHNKT    ! lg(hnu/KT)
      DOUBLE PRECISION ARG       ! Temporary variable

*.

*  Default value for invalid WRT and for x outside profile.
      SPD_UAAVD = 0.

*  With respect to Theta.
      IF ( WRT .EQ. 1 ) THEN
         SPD_UAAVD = 1.

*  With respect to alpha.
      ELSE IF ( WRT .EQ. 2 ) THEN
         SPD_UAAVD = LGNU

*  With respect to lg(T).
      ELSE IF ( WRT .EQ. 3 ) THEN
         LGHNKT = LGHOVK + LGNU - LGTC

*     Rayleigh-Jeans approximation.
         IF ( LGHNKT .LT. -17D0 ) THEN
            SPD_UAAVD = 1.

*     Wien approximation.
         ELSE IF ( LGHNKT .GT. 1.59106D0 ) THEN
            SPD_UAAVD = 1D1 ** LGHNKT

*     Precise derivative of Planck function.
         ELSE
            ARG = 1D1 ** LGHNKT
            SPD_UAAVD = ARG / ( 1D0 - EXP(-ARG) )
         END IF
      END IF

*  Return.
      END
