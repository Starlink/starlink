      SUBROUTINE DAT_DEF ( PARAM, LOC, STATUS )
*+
*  Name:
*     DAT_DEF

*  Purpose:
*     Suggest values for parameter.

*  Language:
*     Fortran 77

*  Invocation:
*     CALL DAT_DEF ( PARAM, LOC, STATUS)

*  Description:
*     Set a data-system object as the dynamic default for a parameter.
*     The given locator must be valid when DAT_DEF is called but may
*     be annulled before the dynamic default is used.

*  Arguments:
*     PARAM=CHARACTER*(*) (given)
*        Name of program parameter.
*     LOC=CHARACTER*(*) (given)
*        Locator to a data object.
*     STATUS=INTEGER (given and returned)
*        Global status

*  Algorithm:
*     Use SUBPAR to get the pointer to the parameter, and store the HDS
*     name of the indicated object.

*  Authors:
*     BDK: B D Kelly (ROE)
*     AJC: A J Chipperfield (Starlink)
*     {enter_new_authors_here}

*  History:
*     10-DEC-1984 (BDK)
*        Original 
*      1-SEP-1993 (AJC)
*        Correctly comment inherited LOC size 
*     16-JUN-1998 (AJC)
*        Re-format prologue
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'                 ! General Symbolic Constants

*  Arguments Given:
      CHARACTER*(*) PARAM		! Parameter name

      CHARACTER*(*) LOC			! Locator to data

*    Status return :
      INTEGER STATUS			! Status return

*  Local Variables:
      INTEGER NAMECODE                  ! pointer to parameter

*.

      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL SUBPAR_FINDPAR ( PARAM, NAMECODE, STATUS )
      CALL SUBPAR_DATDEF ( NAMECODE, LOC, STATUS )

      END
