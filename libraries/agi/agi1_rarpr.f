************************************************************************

      SUBROUTINE AGI_1RARPR ( PICLOC, PTYPE, FOUND, PVAL, STATUS )

*+
*  Name:
*     AGI_1RARPR

*  Purpose:
*     Read a real 4 element array.

*  Language:
*     VAX Fortran

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL AGI_1RARPR( PICLOC, PTYPE, FOUND, PVAL, STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Read the contents of a real 4 element array

*  Algorithm:
*     Check status on entry.
*     If the given parameter is there then
*        Read the contents of the array.
*     Endif

*  Authors:
*     Nick Eaton  ( DUVAD::NE )
*     {enter_new_authors_here}

*  History:
*     July 1988
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'


*  Arguments Given:
*     Locator to picture
      CHARACTER * ( DAT__SZLOC ) PICLOC

*     Name of parameter to read
      CHARACTER * ( * ) PTYPE


*  Arguments Returned:
*     Flag to indicate if parameter has been found
      LOGICAL FOUND

*     Array of values. Undefined if .NOT. FOUND
      REAL PVAL( 4 )


*  Status:
      INTEGER STATUS


*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) PARLOC

*.


      IF ( STATUS .EQ. SAI__OK ) THEN

*   Check if the parameter is present
         PARLOC = ' '
         CALL AGI_1FPAR( PICLOC, PTYPE, PARLOC, FOUND, STATUS )

*   Read contents of element
         IF ( FOUND ) THEN
            CALL DAT_GETR( PARLOC, 1, 4, PVAL, STATUS )
            CALL DAT_ANNUL( PARLOC, STATUS )
            PARLOC = ' '
         ENDIF

      ENDIF

*      print*, '+++++ AGI_1RARPR +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

