      SUBROUTINE TEST_ISALF(STATUS)
*+
*  Name:
*     TEST_ISALF

*  Purpose:
*     Test CHR_ISALF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_ISALF(STATUS)

*  Description:
*     Test CHR_ISALF.
*     If any failure occurs, return STATUS = SAI__ERROR.
*     Otherwise, STATUS is unchanged.

*  Arguments:
*     STATUS = INTEGER (Returned)
*        The status of the tests. 

*  Authors:
*     RLVAD::AJC: A J Chipperfield (STARLINK)
*     RLVAD::ACC: A C Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-AUG-1989 (RLVAD::AJC):
*        Original version.
*     14-SEP-1993 (ACC)
*        Modularised version: broken into one routine for each of 5 main 
*        categories of tests.
*     02-MAR-1994 (ACC)
*        Second modularised version: broken further into one routine for 
*        each of subroutine tested.  This subroutine created.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Subprograms called:    
*     CHR_ISALF

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
*     None

*  Arguments Returned:
      INTEGER STATUS

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CHR_ERR'

*  External References:
      LOGICAL CHR_ISALF

*.

*    Test CHR_ISALF

      IF ((CHR_ISALF('A') .AND. CHR_ISALF('a') .AND.
     :     CHR_ISALF('Z') .AND. CHR_ISALF('z')) .AND.
     :    .NOT.
     :    (CHR_ISALF('0') .OR. CHR_ISALF('9') .OR.
     :     CHR_ISALF('!') .OR. CHR_ISALF('/') .OR.
     :     CHR_ISALF('@'))) THEN
         PRINT *, 'CHR_ISALF OK'
      ELSE
         PRINT *, 'CHR_ISALF FAILS'
         STATUS = SAI__ERROR
      ENDIF

      END
