      SUBROUTINE TEST_CTOI(STATUS)
*+
*  Name:
*     TEST_CTOI

*  Purpose:
*     Test CHR_CTOI.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_CTOI(STATUS)

*  Description:
*     Test CHR_CTOI.
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
*     CHR_CTOI

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
      INTEGER ISTAT              ! Local status
      INTEGER I                  ! INTEGER value
      CHARACTER*120 STRING

*.

*    Test CHR_CTOI

      I = 0
      ISTAT = SAI__OK
      CALL CHR_CTOI ('XXX', I, ISTAT)
      IF (ISTAT .NE. SAI__ERROR) THEN
         PRINT *, 'CHR_CTOI FAILS - Error not detected'
         STATUS = SAI__ERROR
      ENDIF
      ISTAT = SAI__OK
      STRING = '3 -3'
      CALL CHR_CTOI (STRING(3:4), I, ISTAT)
      IF ((ISTAT .EQ. SAI__OK) .AND. (I .EQ. -3)) THEN
         PRINT *, 'CHR_CTOI OK'
      ELSE
         PRINT *, 'CHR_CTOI FAILS - '
         PRINT *, STRING,'read as',I
         STATUS = SAI__ERROR
      ENDIF

      END
