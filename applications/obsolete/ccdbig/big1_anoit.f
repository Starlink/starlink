      SUBROUTINE BIG1_ANOIT( DTYPE, IMAGE, NPIX, ADU, STATUS )
*+
*  Name:
*     BIG1_ANOIT

*  Purpose:
*     To add poissonian noise to data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL BIG1_ANOIT( DTYPE, IMAGE, NPIX, ADU, STATUS )

*  Arguments:
*     DTYPE = CHARACTER * ( * )
*        Data type of the IMAGE array.
*     IMAGE( 1 ) = unknown (Given and Returned)
*        The image to which noise is to be added.
*     NPIX = INTEGER (Given)
*        The size of the array image, note that we can handle
*        n-dimensional arrays.
*     ADU = REAL (Returned)
*        The scaling factor to get the values in IMAGE to their counting
*        values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-NOV-1991 (PDRAPER):
*        Original version.
*     3-JUL-1998 (MBT):
*        Made it generic.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Bad values VAL__BADR

*  Arguments Given:
      CHARACTER * ( * ) DTYPE
      INTEGER NPIX
      REAL ADU

*  Arguments Given and Returned:
      INTEGER IMAGE( 1 )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:

*  Local Variables:

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Call appropriate routine according to data type.
      IF      ( DTYPE .EQ. '_WORD'    ) THEN
         CALL BGG1_ANOIW( IMAGE, NPIX, ADU, STATUS )
      ELSE IF ( DTYPE .EQ. '_INTEGER' ) THEN
         CALL BGG1_ANOII( IMAGE, NPIX, ADU, STATUS )
      ELSE IF ( DTYPE .EQ. '_REAL'    ) THEN
         CALL BGG1_ANOIR( IMAGE, NPIX, ADU, STATUS )
      ELSE IF ( DTYPE .EQ. '_DOUBLE'  ) THEN
         CALL BGG1_ANOID( IMAGE, NPIX, ADU, STATUS )
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'DTYPE', DTYPE )
         CALL ERR_REP( 'CCDBGEN_TYPE',
        :              'CCDBGEN: Invalid data type (^DTYPE)', STATUS )
      END IF

      END
* $Id: ccd1_anoi.f,v 1.1 1997/06/27 09:01:41 pwd Exp $
