      SUBROUTINE TEST_PREFX(STATUS)
*+
*  Name:
*     TEST_PREFX

*  Purpose:
*     Test CHR_PREFX.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_PREFX(STATUS)

*  Description:
*     Test CHR_PREFX.
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
*     CHR_PREFX

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
      INTEGER LENGTH             ! String index
      CHARACTER*120 STRING

*.

*    Test CHR_PREFX

      STRING = 'fix'
      CALL CHR_PREFX( 'Pre', STRING, LENGTH )
      IF ( STRING .EQ. 'Prefix' .AND.
     :     LENGTH .EQ. 6 ) THEN
         PRINT *, 'CHR_PREFX OK'
      ELSE
         PRINT *, 'CHR_PREFX FAILS - STRING is:', STRING
         STATUS = SAI__ERROR
      ENDIF

      END
