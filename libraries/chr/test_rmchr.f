      SUBROUTINE TEST_RMCHR(STATUS)
*+
*  Name:
*     TEST_RMCHR

*  Purpose:
*     Test CHR_RMCHR.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_RMCHR(STATUS)

*  Description:
*     Test CHR_RMCHR.
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
*     CHR_RMCHR

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
      CHARACTER*30 LINE
      CHARACTER*30 RMCLIN

*  Local Data:
      DATA RMCLIN/'   A  C DE                    '/
*.

*    Test CHR_RMCHR

      LINE = ' X  A B XCBB DE B '
      CALL CHR_RMCHR( 'BX', LINE )
      IF ( LINE .EQ. RMCLIN ) THEN
         PRINT *, 'CHR_RMCHR OK'
      ELSE
         PRINT *, 'CHR_RMCHR FAILS - STRING is:', LINE
         PRINT *, 'it should be               :', RMCLIN
         STATUS = SAI__ERROR
      ENDIF

      END
