      SUBROUTINE CCDB1_FLMUL( IMAGE, NCOLS, NLINES, STATUS )
*+
*  Name:
*     CCDB1_FLMUL

*  Purpose:
*     Multiplies an image in place by a ramp.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCDB1_FLMUL( IMAGE, NCOLS, NLINES, STATUS )

*  Notes:
*     Assumes there are no bad pixels.  Since multiplication is by a 
*     value between 0 and 1, this means that no bad pixels will result
*     in the output either.

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-JUN-1998 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NCOLS
      INTEGER NLINES

*  Arguments Given and Returned:
      REAL IMAGE( NCOLS, NLINES )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL JNTER
      INTEGER I, J

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      JNTER  = 1.0 / REAL ( NLINES - 1 )
      DO  J  =  1, NLINES
         DO  I  =  1, NCOLS
            IMAGE( I, J ) = IMAGE( I, J ) 
     :                      * ( 0.5 + ( JNTER * REAL ( J - 1 ) ) )
         END DO
      END DO

      END
* $Id: ccdb1_flmul.f,v 1.2 1998/06/15 15:19:54 mbt Exp $
