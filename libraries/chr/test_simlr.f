      SUBROUTINE TEST_SIMLR(STATUS)
*+
*  Name:
*     TEST_SIMLR

*  Purpose:
*     Test CHR_SIMLR.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_SIMLR(STATUS)

*  Description:
*     Test CHR_SIMLR.
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
*     CHR_SIMLR

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
      LOGICAL CHR_SIMLR

*.

*    Test CHR_SIMLR

      IF (CHR_SIMLR ('AAZZ!@[`{~ ', 'AaZz!@[`{~') .AND.
     :    .NOT. CHR_SIMLR ('AaZz!@[`{~', 'AaZz!@[`{*') .AND.
     :    .NOT. CHR_SIMLR ('AaZz!@[`{~', 'AaZz!@[`{~*')) THEN
         PRINT *, 'CHR_SIMLR OK'
      ELSE
         PRINT *, 'CHR_SIMLR FAILS'
         STATUS = SAI__ERROR
      ENDIF

      END
