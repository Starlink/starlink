      SUBROUTINE TEST_ITOO(STATUS)
*+
*  Name:
*     TEST_ITOO

*  Purpose:
*     Test CHR_ITOO.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_ITOO(STATUS)

*  Description:
*     Test CHR_ITOO.
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
*     CHR_ITOO

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
      INTEGER ISTAT,JSTAT        ! Local status
      CHARACTER*120 STRING

*.

*    Test CHR_ITOO -- Integer to octal character string 

      ISTAT = SAI__OK
      JSTAT = SAI__OK
      STRING = ' '
      CALL CHR_ITOO (86, STRING, ISTAT)
      IF (STRING(1:3) .NE. '126' .OR. ISTAT .NE. SAI__OK) THEN
         PRINT *, 'CHR_ITOO FAILS- STRING is:',STRING
         JSTAT = SAI__ERROR
      ENDIF

      STRING = ' '
      CALL CHR_ITOO (-1, STRING, ISTAT)
      IF (STRING(1:11) .NE. '37777777777' .OR. ISTAT .NE. SAI__OK) THEN
         PRINT *, 'CHR_ITOO FAILS- STRING is:',STRING
         JSTAT = SAI__ERROR
      ENDIF

      IF (JSTAT .EQ. SAI__OK) THEN
         PRINT *, 'CHR_ITOO OK'
      ELSE
         STATUS = SAI__ERROR
      ENDIF

      END
