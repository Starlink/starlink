      SUBROUTINE BIG1_FLMULT( DTYPE, IMAGE, NCOLS, NLINES, STATUS )
*+
*  Name:
*     BIG1_FLMULT

*  Purpose:
*     Multiplies an image in place by a ramp.
*     This routine is a harness for the BGG1_FLMUL<T> routines.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL BIG1_FLMULT( DTYPE, IMAGE, NCOLS, NLINES, STATUS )

*  Arguments:
*     DTYPE = CHARACTER * ( * )
*        Data type of the IMAGE array.
*     IMAGE( 1 ) = unknown array (Given and returned)
*        The image to be multiplied by the flatfield.
*     NCOLS = INTEGER (Given)
*        Number of columns in IMAGE.
*     NLINES = INTEGER (Given)
*        Number of lines in IMAGE.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:

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
      CHARACTER * ( * ) DTYPE    
      INTEGER NCOLS
      INTEGER NLINES

*  Arguments Given and Returned:
      REAL IMAGE( NCOLS, NLINES )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Call appropriate routine according to data type.
      IF      ( DTYPE .EQ. '_WORD'    ) THEN
         CALL BGG1_FLMULW( IMAGE, NCOLS, NLINES, STATUS )
      ELSE IF ( DTYPE .EQ. '_INTEGER' ) THEN
         CALL BGG1_FLMULI( IMAGE, NCOLS, NLINES, STATUS )
      ELSE IF ( DTYPE .EQ. '_REAL'    ) THEN
         CALL BGG1_FLMULR( IMAGE, NCOLS, NLINES, STATUS )
      ELSE IF ( DTYPE .EQ. '_DOUBLE'  ) THEN
         CALL BGG1_FLMULD( IMAGE, NCOLS, NLINES, STATUS )
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'DTYPE', DTYPE )
         CALL ERR_REP( 'CCDBGEN_TYPE',
     :                 'CCDBGEN: Invalid data type (^DTYPE)', STATUS )
      END IF

      END
* $Id: big1_flmult.f,v 1.2 1998/07/03 15:37:42 mbt Exp $
