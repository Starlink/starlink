
      SUBROUTINE CCD1_CFF( IMAGE, NCOLS, NLINES, STATUS )
*+
*  Name:
*     CCD1_CFF

*  Purpose:
*     Creates a ramped image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_CFF( IMAGE, NCOLS, NLINES, STATUS )

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-NOV-1991 (PDRAPER):
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
      REAL INTER
      INTEGER I, J

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      INTER  = 1.0 / ( NLINES - 1 )
      DO  J  =  1, NLINES
         DO  I  =  1, NCOLS
            IMAGE( I, J ) = 0.5 +  ( INTER *  ( I - 1 ) )
         END DO
      END DO

      END
* $Id$
