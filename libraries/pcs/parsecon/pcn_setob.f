      SUBROUTINE PARSECON_SETOB ( FLAG, STATUS )
*+
*  Name:
*     PARSECON_SETOB

*  Purpose:
*     Set OBEY flag.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_SETOB ( FLAG, STATUS )

*  Description:
*     Sets the flag indicating whether OBEY is valid for the current
*     action

*  Arguments:
*     FLAG=LOGICAL (given)
*        .TRUE. => OBEY is valid for the current action.
*     STATUS=INTEGER

*  Algorithm:
*     Set the value into the common-block variable.

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
      LOGICAL FLAG                 ! value to be set


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*.


      IF ( STATUS .NE. SAI__OK ) RETURN

      MAYOB(ACTPTR) = FLAG

      END
