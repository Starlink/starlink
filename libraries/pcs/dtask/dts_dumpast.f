      SUBROUTINE DTASK_DUMPAST ( ASTPARM )
*+
*  Name:
*     DTASK_DUMPAST

*  Purpose:
*     Dummy for Unix: AST routine for generating a task stack dump

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     An AST routine, invoked by VMS.

*  Description:
*     Generate a stack dump of a task, then reenable the dumping 
*     facility.

*  Arguments:
*     ASTPARM=INTEGER (given)
*           the AST parameter. Unused.

*  Algorithm:
*     Call LIB$SIGNAL to generate a stack dump. Then call DTASK_SETDUMP 
*     to reenable the AST.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     13-AUG-1986 (REVAD::BDK):
*        Original
*     27-AUG-1986 (REVAD::BDK):
*        Use DTASK__DUMP in the signal
*     25-APR-1991 (REVAD::BDK):
*        Revise INCLUDE files
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DTASK_ERR'

*  Arguments Given:
      INTEGER ASTPARM     ! the AST parameter.

*  Local Variables:
      INTEGER STATUS
*.

!      CALL LIB$SIGNAL ( %VAL(DTASK__DUMP) )

      STATUS = SAI__OK
      CALL DTASK_SETDUMP ( STATUS )

      END
