      SUBROUTINE TEST_LOWER(STATUS)
*+
*  Name:
*     TEST_LOWER

*  Purpose:
*     Test CHR_LOWER.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_LOWER(STATUS)

*  Description:
*     Test CHR_LOWER.
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
*     CHR_LOWER

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
      CHARACTER*1 CHR_LOWER

*.

*    Test CHR_LOWER

      IF ((CHR_LOWER('a') .EQ. 'a') .AND.
     :    (CHR_LOWER('A') .EQ. 'a') .AND.
     :    (CHR_LOWER('z') .EQ. 'z') .AND.
     :    (CHR_LOWER('Z') .EQ. 'z') .AND.
     :    (CHR_LOWER('!') .EQ. '!') .AND.
     :    (CHR_LOWER('@') .EQ. '@') .AND.
     :    (CHR_LOWER('[') .EQ. '[') .AND.
     :    (CHR_LOWER('`') .EQ. '`') .AND.
     :    (CHR_LOWER('{') .EQ. '{') .AND.
     :    (CHR_LOWER('~') .EQ. '~')) THEN
         PRINT *, 'CHR_LOWER OK'
      ELSE
         PRINT *, 'CHR_LOWER FAILS'
         STATUS = SAI__ERROR
      ENDIF

      END
