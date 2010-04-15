      SUBROUTINE SPD_UAABR( NELM, NEWBAD, ARRAY, STATUS )
*+
*  Name:
*     SPD_UAAB{DR}

*  Purpose:
*     Replace bad values with constant.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_UAABR( NELM, NEWBAD, ARRAY, STATUS )

*  Description:
*     This routine works on an array in situ and replaces all existing
*     bad values with the specified value.

*  Arguments:
*     NELM = INTEGER (Given)
*        The size of the array.
*     NEWBAD = REAL (Given)
*        The (presumably non-bad) value to be used to replace bad
*        values.
*     ARRAY( NELM ) = REAL (Given and Returned)
*        The array to be processed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     05 May 1994 (hme):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Standard PRIMDAT constants

*  Arguments Given:
      INTEGER NELM
      REAL NEWBAD

*  Arguments Given and Returned:
      REAL ARRAY( NELM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop index

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      DO 1 I = 1, NELM
         IF ( ARRAY(I) .EQ. VAL__BADR ) ARRAY(I) = NEWBAD
 1    CONTINUE

      END
