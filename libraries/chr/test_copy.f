      SUBROUTINE TEST_COPY(STATUS)
*+
*  Name:
*     TEST_COPY

*  Purpose:
*     Test CHR_COPY.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_COPY(STATUS)

*  Description:
*     Test CHR_COPY.
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
*     CHR_COPY

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
      INTEGER I                  ! INTEGER value
      CHARACTER*10 CHARS         ! Strings

*.

*    Test CHR_COPY

      CALL CHR_COPY ('A z09  !@12', .TRUE., CHARS, I)
      IF ((CHARS .EQ. 'A z09  !@#') .AND. (I .EQ. 1)) THEN
         PRINT *, 'CHR_COPY OK'
      ELSE
         PRINT *, 'CHR_COPY FAILS - STRING IS:', CHARS, ' I is ', I
         STATUS = SAI__ERROR
      ENDIF

      END
