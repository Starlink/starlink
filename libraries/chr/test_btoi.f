      SUBROUTINE TEST_BTOI(STATUS)
*+
*  Name:
*     TEST_BTOI

*  Purpose:
*     Test CHR_BTOI.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_BTOI(STATUS)

*  Description:
*     Test CHR_BTOI.
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
*     CHR_BTOI

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
      INTEGER I                  ! INTEGER values
      CHARACTER*120 STRING

*.

*    Test CHR_BTOI

      I = 0
      ISTAT = SAI__OK
      JSTAT = SAI__OK
      CALL CHR_BTOI ('XXX', I, ISTAT)
      IF (ISTAT .NE. SAI__ERROR) THEN
         PRINT *, 'CHR_BTOI FAILS - Error not detected'
         JSTAT = SAI__ERROR
      ENDIF

      ISTAT = SAI__OK
      STRING = '01010110'
      CALL CHR_BTOI (STRING, I, ISTAT)
      IF ((ISTAT .NE. SAI__OK) .OR. (I .NE. 86)) THEN
         PRINT *, 'CHR_BTOI FAILS - '
         PRINT *, STRING,'read as',I
         JSTAT = SAI__ERROR
      ENDIF

      ISTAT = SAI__OK
      STRING = '11111111111111111111111110101010'
      CALL CHR_BTOI (STRING(1:32), I, ISTAT)
      IF ((ISTAT .NE. SAI__OK) .OR. (I .NE. -86)) THEN
         PRINT *, 'CHR_BTOI FAILS - '
         PRINT *, STRING,'read as',I
         JSTAT = SAI__ERROR
      ENDIF

      IF (JSTAT .EQ. SAI__OK) THEN
         PRINT *, 'CHR_BTOI OK'
      ELSE
         STATUS = SAI__ERROR
      END IF

      END
