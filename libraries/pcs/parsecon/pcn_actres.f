      SUBROUTINE PARSECON_ACTRES ( FLAG, STATUS )
*+
*  Name:
*     PARSECON_ACTRES

*  Purpose:
*     Set flag indicating RANGE or IN limit on action.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_ACTRES ( FLAG, STATUS )

*  Description:
*     Sets the flag indicating whether the NEEDS parameter being defined
*     for the current action has a RANGE or an IN constraint.

*  Arguments:
*     FLAG=LOGICAL (given)
*        .TRUE. => RANGE constraint on the value of the parameter
*        required for the action currently being defined.
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

      NEEDCONT(NEEDPTR) = FLAG

      END
