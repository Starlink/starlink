      SUBROUTINE KPS1_LINV( TR, TRINV, STATUS )
*+
*  Name:
*     KPS1_LINV

*  Purpose:
*     Finds the inverse of a linear transformation.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_LINV( TR, TRINV, STATUS )

*  Description:
*     This routine determines the inverse transformation of
*     a six-parameter linear fit of the type:
*
*        XDASH = TR( 1 ) + TR( 2 ) * X + TR( 3 ) * Y
*        YDASH = TR( 4 ) + TR( 5 ) * X + TR( 6 ) * Y
*
*     The inverse is simply:
*
*        X = TRINV( 1 ) + TRINV( 2 ) * XDASH + TRINV( 3 ) * YDASH
*        Y = TRINV( 4 ) + TRINV( 5 ) * XDASH + TRINV( 6 ) * YDASH
*
*     where
*
*        TRINV( 1 ) = ( TR( 4 ) * TR( 3 ) - TR( 1 ) * TR( 6 ) ) / DET
*        TRINV( 2 ) = TR( 6 ) / DET
*        TRINV( 3 ) = -TR( 3 ) / DET
*        TRINV( 4 ) = ( TR( 1 ) * TR( 5 ) - TR( 4 ) * TR( 2 ) ) / DET
*        TRINV( 5 ) = -TR( 5 ) / DET
*        TRINV( 6 ) = TR( 2 ) / DET
*
*     and DET is the determinant given by
*
*         DET = TR( 2 ) * TR( 6 ) - TR( 3 ) * TR( 5 )

*  Arguments:
*     TR( 6 ) = DOUBLE PRECISION (Given)
*        The linear transformation whose inverse is required.
*     TRINV( 6 ) = DOUBLE PRECISION (Returned)
*        The inverse transformation.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If the determinant of the input transformation is zero then
*     an error will be reported.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27-JUL-1992 (PDRAPER):
*        Original version based on EDRS version by R.F. Warren-Smith
*     1995 February 26 (MJC):
*        Renamed and some typo's corrected for use in KAPPA.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      DOUBLE PRECISION TR( 6 )

*  Arguments Returned:
      DOUBLE PRECISION TRINV( 6 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION DET

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Form determinant and determine if the transformation is singular.
      DET = TR( 2 ) * TR( 6 ) - TR( 3 ) * TR( 5 )
      IF ( DET .EQ. 0.0D0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_LINV',
     :     'Transformation is singular and cannot be inverted', STATUS )
      ELSE

*  If not, calculate the inverse.
         TRINV( 1 ) = ( TR( 4 ) * TR( 3 ) - TR( 1 ) * TR( 6 ) ) / DET
         TRINV( 2 ) = TR( 6 ) / DET
         TRINV( 3 ) = -TR( 3 ) / DET
         TRINV( 4 ) = ( TR( 1 ) * TR( 5 ) - TR( 4 ) * TR( 2 ) ) / DET
         TRINV( 5 ) = -TR( 5 ) / DET
         TRINV( 6 ) = TR( 2) / DET
      END IF

      END
