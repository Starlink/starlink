      SUBROUTINE MAPCC4( SIZE, DATA, NORM, VAR, BAD, STATUS )
*+
*  Name:
*     MAPCC4

*  Purpose:
*     Calculate output INTERNAL variances.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MAPCC4( SIZE, DATA, NORM, VAR, BAD, STATUS )

*  Description:
*     The vector VAR is divided element-by-element by the vector NORM
*     and the square of vector DATA subtracted from the result. The
*     final values are put back in vector VAR. If NORM contains zero,
*     then the corresponding element of VAR is set bad.  Argument BAD
*     is returned true if any bad elements are returned in VAR. Any
*     negative or zero variances are set bad.

*  Arguments:
*     SIZE = INTEGER (Given)
*        The size of the two vectors.
*     DATA( SIZE ) = REAL (Given)
*        The normalised output surface brightness data.
*     NORM( SIZE ) = REAL (Given)
*        The vector containing the normalisation factors.
*     VAR( SIZE ) = REAL (Given and Returned)
*        The vector holding the variances to be normalised.
*     BAD = LOGICAL (Returned)
*        True if any bad values are returned in VAR.
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
      REAL DATA( SIZE )
      REAL NORM( SIZE )

*  Arguments Given and Returned:
      REAL VAR( SIZE )

*  Arguments Returned:
      LOGICAL BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop count.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      BAD = .FALSE.

      DO I = 1, SIZE
         IF( NORM( I ) .GT. 0.0 .AND. DATA( I ) .NE. VAL__BADR ) THEN
            VAR( I ) = ( VAR( I )/NORM( I ) ) - DATA( I )**2

            IF( VAR( I ) .LE. 0.0 ) THEN
               VAR( I ) = VAL__BADR
               BAD = .TRUE.
            END IF

         ELSE
            VAR( I ) = VAL__BADR
            BAD = .TRUE.

         END IF

      END DO

      END
