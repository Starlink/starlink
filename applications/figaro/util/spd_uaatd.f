      DOUBLE PRECISION FUNCTION SPD_UAATD( CEN, PEA, WID, X )
*+
*  Name:
*     SPD_UAAT{DR}

*  Purpose:
*     Get one value for triangular profile.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = SPD_UAATD( CEN, PEA, WID, X )

*  Description:
*     This function returns for one given abscissa value the ordinate
*     value of a triangular profile.

*  Arguments:
*     CEN = DOUBLE PRECISION (Given)
*        The abscissa value of the centre of the profile.
*     PEA = DOUBLE PRECISION (Given)
*        The ordinate value of the centre of the profile.
*     WID = DOUBLE PRECISION (Given)
*        The full width at half maximum of the profile. As it happens
*        this is also the half width at zero. WID must not equal zero.
*     X = DOUBLE PRECISION (Given)
*        The abscissa value for which the profile's ordinate value is to
*        be returned.

*  Returned Value:
*     SPD_UAATD = DOUBLE PRECISION
*        The ordinate value of the profile at the given abscissa value.

*  Authors:
*     ajlf: Amadeu Fernandes (UoE)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     31 Jan 1992 (ajlf):
*        Original version (FUNC_Y in AMFUN.FOR).
*     24 Jul 1992 (hme):
*        Prologue. Generic routine.
*     19 Dec 1994 (hme):
*        Renamed from SPAAZD.
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

*.

*  Function value.
      IF ( X .GT. CEN - WID .AND. X .LE. CEN ) THEN
         SPD_UAATD = PEA * ( 1. - ( CEN - X ) / WID )
      ELSE IF ( X .GE. CEN .AND. X .LT. CEN + WID ) THEN
         SPD_UAATD = PEA * ( 1. - ( X - CEN ) / WID )
      ELSE
         SPD_UAATD = 0.
      END IF

*  Return.
      END
