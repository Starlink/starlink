      SUBROUTINE TEST_ISNAM(STATUS)
*+
*  Name:
*     TEST_ISNAM

*  Purpose:
*     Test CHR_ISNAM.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_ISNAM(STATUS)

*  Description:
*     Test CHR_ISNAM.
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
*     CHR_ISNAM

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
      LOGICAL CHR_ISNAM

*.

*    Test CHR_ISNAM

      IF (CHR_ISNAM('NAME01') .AND. .NOT.
     : (CHR_ISNAM('01NAME') .OR. CHR_ISNAM('NAME@'))) THEN
         PRINT *, 'CHR_ISNAM OK'
      ELSE
         PRINT *, 'CHR_ISNAM FAILS'
         STATUS = SAI__ERROR
      ENDIF

      END
