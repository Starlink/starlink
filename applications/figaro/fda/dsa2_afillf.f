      SUBROUTINE DSA2_AFILLF( EL, LBND, ARRAY, STATUS )
*+
*  Name:
*     DSA2_AFILLF

*  Purpose:
*     FillS an NDF axis centre array with default values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA2_AFILLF( EL, LBND, ARRAY, STATUS )

*  Description:
*     This routine fills the given array with either numbers that start
*     at LBND - 0.5 or at LBND, depending on which default axis system
*     is chosen, and both increase by 1.0 with each index increment.

*  Arguments:
*     EL = INTEGER (Given)
*        The size of the array to fill.
*     LBND = INTEGER (Given)
*        The lower bound of the NDF.
*     ARRAY( EL ) = REAL (Returned)
*        The array to fill.
*     STATUS = INTEGER (Given & Returned)
*        The global status.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 July 9 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER EL
      INTEGER LBND

*  Arguments Returned:
      REAL ARRAY( EL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter
      LOGICAL PIXIND             ! Pixel indices required?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start a new error context.
      CALL ERR_MARK

*  Determine the type of default axes to create.
      CALL DSA2_PIXIN( PIXIND, STATUS )

*  If an error occurred, use the Starlink standard, once the error is
*  annulled.  (Are we allowed to flush the error stack?)
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         PIXIND = .FALSE.
      END IF

*  Release the error context.
      CALL ERR_RLSE

*  Fill the array in the desired way.
      IF ( PIXIND ) THEN
         DO 10 I = 1, EL
            ARRAY( I ) = REAL( I + LBND - 1 )
   10    CONTINUE

      ELSE
         DO 20 I = 1, EL
            ARRAY( I ) = REAL( I + LBND - 1 ) - 0.5
   20    CONTINUE
      END IF

      END
