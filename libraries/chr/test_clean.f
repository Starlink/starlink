      SUBROUTINE TEST_CLEAN(STATUS)
*+
*  Name:
*     TEST_CLEAN

*  Purpose:
*     Test CHR_CLEAN.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_CLEAN(STATUS)

*  Description:
*     Test CHR_CLEAN.
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
*     CHR_CLEAN

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
      CHARACTER*10 STARS

*.

*    Test CHR_CLEAN

      STARS = '*****'
      CALL CHR_CLEAN (STARS)
      IF (STARS .EQ. '*****') THEN
         PRINT *, 'CHR_CLEAN OK'
      ELSE
         PRINT *, 'CHR_CLEAN FAILS - STRING is:',STARS
         STATUS = SAI__ERROR
      ENDIF
      
      END
