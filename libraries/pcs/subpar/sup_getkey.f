      SUBROUTINE SUBPAR_GETKEY ( NAMECODE, KEYWORD, STATUS )
*+
*  Name:
*     SUBPAR_GETKEY

*  Purpose:
*     get a parameter's keyword.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_GETKEY ( NAMECODE, KEYWORD, STATUS )

*  Description:
*     Return the keyword associated with the indicated program
*     parameter.

*  Arguments:
*     NAMECODE=INTEGER (given)
*        code number of parameter
*     KEYWORD=CHARACTER*(*) (returned)
*        keyword for parameter
*     STATUS=INTEGER

*  Algorithm:
*     Copy the keyword from the SUBPAR common blocks.

*  Authors:
*     BDK: B D Kelly (ROE)
*     {enter_new_authors_here}

*  History:
*     13-NOV-1984 (BDK):
*        Original version
*      1-MAR-1993 (AJC):
*        Add INCLUDE DAT_PAR
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'


*  Arguments Given:
      INTEGER NAMECODE                ! pointer to parameter


*  Arguments Returned:
      CHARACTER*(*) KEYWORD           ! parameter keyword


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Copy the value from storage
*
      KEYWORD = PARKEY(NAMECODE)

      END
