      REAL FUNCTION SPD_UAADR( CEN, PEA, WID, X )
*+
*  Name:
*     SPD_UAAD{DR}

*  Purpose:
*     Get one value for Gauss profile.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = SPD_UAADR( CENTRE, PEAK, FWHM, X )

*  Description:
*     This function returns for one given abscissa value the ordinate
*     value of a Gauss profile.

*  Arguments:
*     CENTRE = REAL (Given)
*        The abscissa value of the centre of the profile.
*     PEAK = REAL (Given)
*        The ordinate value of the centre of the profile.
*     FWHM = REAL (Given)
*        The full width at half maximum of the profile. This must not
*        equal zero.
*     X = REAL (Given)
*        The abscissa value for which the profile's ordinate value is to
*        be returned.

*  Returned Value:
*     SPD_UAADR = REAL
*        The ordinate value of the profile at the given abscissa value.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     27 Jul 1992 (hme):
*        Adapted from SPAAZR.
*     09 May 1994 (hme):
*        Copied from SPACNR.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      REAL CEN
      REAL PEA
      REAL WID
      REAL X

*  Local Constants:
      REAL RT8LN2              ! SQRT( 8 LN(2) )
      PARAMETER ( RT8LN2 = 2.3548200450309 )

*  Local Variables:
      REAL ARG

*.

*  Function value.
      ARG = RT8LN2 * ( X - CEN ) / WID
      SPD_UAADR = PEA * EXP( -0.5 * ARG * ARG )

*  Return.
      END
