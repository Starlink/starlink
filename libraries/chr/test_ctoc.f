      SUBROUTINE TEST_CTOC(STATUS)
*+
*  Name:
*     TEST_CTOC

*  Purpose:
*     Test CHR_CTOC.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_CTOC(STATUS)

*  Description:
*     Test CHR_CTOC.
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
*     01-MAR-1994 (ACC)
*        Second modularised version: broken further into one routine for 
*        each of subroutine tested.  This subroutine created.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Subprograms called:   
*     CHR_CTOC

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

*  Local Variables:
      INTEGER PTR1               ! String indexes
      CHARACTER*10 CHARS         ! Strings

*.

*    Test CHR_CTOC -- Character to character

      CALL CHR_CTOC ('WXYZ   ', CHARS, PTR1)
      IF ((CHARS .EQ. 'WXYZ') .AND. (PTR1 .EQ. 4)) THEN
         PRINT *, 'CHR_CTOC OK'
      ELSE
         PRINT *, 'CHR_CTOC FAILS'
         STATUS = SAI__ERROR
      ENDIF

      END
