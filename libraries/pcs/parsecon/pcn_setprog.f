      SUBROUTINE PARSECON_SETPROG ( NAME, STATUS )
*+
*  Name:
*     PARSECON_SETPROG

*  Purpose:
*     Set program name.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_SETPROG ( NAME, STATUS )

*  Description:
*     Sets program name into common block.

*  Arguments:
*     NAME=CHARACTER*(*) (given)
*        program name
*     STATUS=INTEGER

*  Algorithm:
*     Put the given string into the common-block variable.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     02.10.1984:  Original (REVAD::BDK)
*     24.03.1993:  Add DAT_PAR for SUBPAR_CMN
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
      CHARACTER*(*) NAME                 ! string to be inserted


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*.


      IF ( STATUS .NE. SAI__OK ) RETURN

      PROGNAME = NAME

      END
