      SUBROUTINE MAPCC2( SIZE, NORM, DATA, BAD, STATUS )
*+
*  Name:
*     MAPCC2

*  Purpose:
*     Normalise the output data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MAPCC2( SIZE, NORM, DATA, BAD, STATUS )

*  Description:
*     The vector DATA is divided element-by-element by vector NORM and
*     the result put back in vector DATA. If NORM contains zero, then
*     the corresponding element of DATA is set bad. Argument BAD is
*     returned true if any bad elements are returned in DATA.

*  Arguments:
*     SIZE = INTEGER (Given)
*        The size of the two vectors.
*     NORM( SIZE ) = REAL (Given)
*        The vector containing the normalisation factors.
*     DATA( SIZE ) = REAL (Given and Returned)
*        The vector holding the data to be normalised.
*     BAD = LOGICAL (Returned)
*        True if any bad values are returned in DATA.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-JAN-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants.

*  Arguments Given:
      INTEGER SIZE
      REAL NORM( SIZE )

*  Arguments Given and Returned:
      REAL DATA( SIZE )

*  Arguments Returned:
      LOGICAL BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL DENOM                 ! The denominator value.
      INTEGER I                  ! Loop count.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      BAD = .FALSE.

      DO I = 1, SIZE
         DENOM = NORM( I )

         IF( DENOM .NE. 0.0 ) THEN
            DATA( I ) = DATA( I )/DENOM

         ELSE
            DATA( I ) = VAL__BADR
            BAD = .TRUE.

         END IF

      END DO

      END
