      SUBROUTINE TEST_FILL(STATUS)
*+
*  Name:
*     TEST_FILL

*  Purpose:
*     Test CHR_FILL.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_FILL(STATUS)

*  Description:
*     Test CHR_FILL.
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
*     CHR_FILL

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
      CHARACTER*10 CHARS         ! Strings
      CHARACTER*10 STARS

*.

*    Test CHR_FILL

*    Fill a string with *
      CHARS = '          '
      STARS = '**********'
      CALL CHR_FILL ('*', CHARS)
      IF (CHARS .EQ. STARS) THEN
         PRINT *, 'CHR_FILL OK'
      ELSE
         PRINT *, 'CHR_FILL FAILS - STRING is:', CHARS
         STATUS = SAI__ERROR
      ENDIF

      END
