      SUBROUTINE PARSECON_MESTEXT ( TEXT, STATUS )
*+
*  Name:
*     PARSECON_MESTEXT

*  Purpose:
*     Stores text for a message parameter.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_MESTEXT ( TEXT, STATUS )

*  Description:
*     The parameter currently being defined is a message parameter.
*     Set-up the parameter as a message, and store the message text.

*  Arguments:
*     TEXT=CHARACTER*(*) (given)
*        text of the message
*     STATUS=INTEGER

*  Algorithm:
*     Mark the most-recently defined parameter for READ access, set
*     VPATH to INTERNAL, set the type to character, and put the text
*     string into the static default.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     02.10.1984
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
      INCLUDE 'SUBPAR_PAR'


*  Arguments Given:
      CHARACTER*(*) TEXT              ! text of message


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Set type
*
      PARTYPE(PARPTR) = SUBPAR__CHAR
*
*   Set text as static default
*
      CALL PARSECON_SETDEF ( TEXT, STATUS )
*
*   Mark parameter for READ access and INTERNAL VPATH
*
      PARWRITE(PARPTR) = .FALSE.
      PARVPATH(1,PARPTR) = SUBPAR__INTERNAL

      END
