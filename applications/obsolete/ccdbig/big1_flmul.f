      SUBROUTINE CCD1_FLMUL( IMAGE, NCOLS, NLINES, STATUS )
*+
*  Name:
*     CCD1_FLMUL

*  Purpose:
*     Multiplies an image in place by a ramp.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_FLMUL( IMAGE, NCOLS, NLINES, STATUS )

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
* $Id: ccd1_cff.f,v 1.1 1997/06/27 09:01:41 pwd Exp $
