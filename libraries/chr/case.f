      SUBROUTINE TEST_CASE(STATUS)
*+
*  Name:
*     TEST_CASE

*  Purpose:
*     Test the case conversion routines.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_CASE(STATUS)

*  Description:
*     Test each of the case conversion routines listed in Appendix A.3 
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
*     TEST_LCASE, TEST_LOWER, TEST_UCASE, TEST_UPPER  

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
      PRINT *,'*** Test change case routines ***'

      STATUS = SAI__OK

*    Test CHR_LCASE

      ISTAT = SAI__OK
      CALL TEST_LCASE(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_LOWER

      ISTAT = SAI__OK
      CALL TEST_LOWER(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_UCASE

      ISTAT = SAI__OK
      CALL TEST_UCASE(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_UPPER

      ISTAT = SAI__OK
      CALL TEST_UPPER(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Write summary message

      IF (STATUS .EQ. SAI__OK) THEN 
         PRINT *,'*** All change case routines OK ***'
      ELSE
         PRINT *,'*** Error(s) in change case routines ***'
      END IF

      END
