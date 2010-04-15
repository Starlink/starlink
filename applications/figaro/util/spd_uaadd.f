      DOUBLE PRECISION FUNCTION SPD_UAADD( CEN, PEA, WID, X )
*+
*  Name:
*     SPD_UAAD{DR}

*  Purpose:
*     Get one value for Gauss profile.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = SPD_UAADD( CENTRE, PEAK, FWHM, X )

*  Description:
*     This function returns for one given abscissa value the ordinate
*     value of a Gauss profile.

*  Arguments:
*     CENTRE = DOUBLE PRECISION (Given)
*        The abscissa value of the centre of the profile.
*     PEAK = DOUBLE PRECISION (Given)
*        The ordinate value of the centre of the profile.
*     FWHM = DOUBLE PRECISION (Given)
*        The full width at half maximum of the profile. This must not
*        equal zero.
*     X = DOUBLE PRECISION (Given)
*        The abscissa value for which the profile's ordinate value is to
*        be returned.

*  Returned Value:
*     SPD_UAADD = DOUBLE PRECISION
*        The ordinate value of the profile at the given abscissa value.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     27 Jul 1992 (hme):
*        Adapted from SPAAZD.
*     09 May 1994 (hme):
*        Copied from SPACND.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      DOUBLE PRECISION CEN
      DOUBLE PRECISION PEA
      DOUBLE PRECISION WID
      DOUBLE PRECISION X

*  Local Constants:
      DOUBLE PRECISION RT8LN2              ! SQRT( 8 LN(2) )
      PARAMETER ( RT8LN2 = 2.3548200450309 )

*  Local Variables:
      DOUBLE PRECISION ARG

*.

*  Function value.
      ARG = RT8LN2 * ( X - CEN ) / WID
      SPD_UAADD = PEA * EXP( -0.5 * ARG * ARG )

*  Return.
      END
