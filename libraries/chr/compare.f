      SUBROUTINE TEST_COMPARE(STATUS)
*+
*  Name:
*     TEST_COMPARE

*  Purpose:
*     Test the string comparison routines.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_COMPARE(STATUS)

*  Description:
*     Test each of the string comparison routines listed in Appendix A.3 
*     of SUN/40.3.
*     If any failure occurs, return STATUS = SAI__ERROR.
*     Otherwise, STATUS is unchanged.

*  Arguments:
*     STATUS = INTEGER (Returned)
*        The status of the tests. 

*  Authors:
*     RLVAD::ACC: A C Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-SEP-1993 (ACC)
*        Original version.
*     02-MAR-1994 (ACC)
*        Broke into separate routines for each routine tested.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Subprograms called:    
*     TEST_ABBRV, TEST_EQUAL, TEST_SCOMP, TEST_SIMLR, TEST_WILD

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

*.

      PRINT *,' '
      PRINT *,'*** Test compare strings routines ***'

      STATUS = SAI__OK

*    Test CHR_ABBRV

      ISTAT = SAI__OK
      CALL TEST_ABBRV(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_EQUAL

      ISTAT = SAI__OK
      CALL TEST_EQUAL(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_SCOMP

      ISTAT = SAI__OK
      CALL TEST_SCOMP(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_SIMLR

      ISTAT = SAI__OK
      CALL TEST_SIMLR(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_WILD

      ISTAT = SAI__OK
      CALL TEST_WILD(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Write summary message

      IF (STATUS .EQ. SAI__OK) THEN 
         PRINT *,'*** All compare strings routines OK ***'
      ELSE
         PRINT *,'*** Error(s) in compare strings routines'
      END IF

      END
