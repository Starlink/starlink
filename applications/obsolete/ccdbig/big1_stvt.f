      SUBROUTINE BIG1_STVT( DTYPE, VAL, SIZE, ARR, STATUS )
*+
*  Name:
*     BIG1_STVT

*  Purpose:
*     To set the elements of a vectorised array to a given value.
*     This routine is a harness for the BGG1_STV<T> routines.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL BIG1_STVT( DTYPE, VAL, SIZE, ARR, STATUS )

*  Description:
*     Loops over array setting the elements to the given value.

*  Arguments:
*     DTYPE = CHARACTER * ( * )
*        Data type of the IMAGE array.
*     VAL = REAL (Given)
*        The value to set array to.
*     SIZE = INTEGER (Given)
*        Size of the array.
*     ARR( 1 ) = unknown (Returned)
*        The array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-MAY-1991 (PDRAPER):
*        Original version.
*     3-JUL1998 (MBT)
*        Rewritten as harness for generic routines.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      REAL VAL
      INTEGER SIZE

*  Arguments Returned:
      INTEGER ARR( 1 )

*  External references:

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Call appropriate routine according to data type.
      IF      ( DTYPE .EQ. '_WORD'    ) THEN
         CALL BIG1_STVW( VAL, SIZE, ARR, STATUS )
      ELSE IF ( DTYPE .EQ. '_INTEGER' ) THEN
         CALL BIG1_STVI( VAL, SIZE, ARR, STATUS )
      ELSE IF ( DTYPE .EQ. '_REAL'    ) THEN
         CALL BIG1_STVR( VAL, SIZE, ARR, STATUS )
      ELSE IF ( DTYPE .EQ. '_DOUBLE'  ) THEN
         CALL BIG1_STVD( VAL, SIZE, ARR, STATUS )
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'DTYPE', DTYPE )
         CALL ERR_REP( 'CCDBGEN_TYPE',
        :              'CCDBGEN: Invalid data type (^DTYPE)', STATUS )
      END IF

      END
* $Id: bgg1_stv.gen,v 1.2 1998/07/03 13:28:28 mbt Exp $
