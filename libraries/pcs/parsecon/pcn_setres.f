      SUBROUTINE PARSECON_SETRES ( FLAG, STATUS )
*+
*  Name:
*     PARSECON_SETRES

*  Purpose:
*     Set flag indicating RANGE or IN restrictions.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_SETRES ( FLAG, STATUS )

*  Description:
*     Sets the flag indicating whether the constraints being defined for
*     the most-recently declared paramater are for RANGE or IN.

*  Arguments:
*     FLAG=LOGICAL (given)
*        .TRUE. => RANGE constraint
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

      PARCONT(PARPTR) = FLAG

      END
