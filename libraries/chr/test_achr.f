      SUBROUTINE TEST_ACHR(STATUS)
*+
*  Name:
*     TEST_ACHR

*  Purpose:
*     Test CHR_ACHR.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_ACHR(STATUS)

*  Description:
*     Test CHR_ACHR.
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
*     CHR_ACHR

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

*  External References:
      CHARACTER*1 CHR_ACHR

*  Local Constants:
      CHARACTER*1 NUL            ! Null character
      INTEGER A_NUL
      PARAMETER (A_NUL = 0)
      INTEGER A_SPACE
      PARAMETER (A_SPACE = 32)
      INTEGER A_INVALID
      PARAMETER (A_INVALID = 132)

*  Local Variables:
      INTEGER ISTAT              ! Local status
      INTEGER ICHR               ! INTEGER value
      CHARACTER*1 CHAR1          ! Strings

*.

      NUL = CHAR(A_NUL)

*    Test CHR_ACHR

      ISTAT = SAI__OK
      ICHR = A_SPACE
      CHAR1 = CHR_ACHR ( ICHR )
      IF ( CHAR1 .NE. ' ' ) THEN
         PRINT *, 
     :   'CHR_ACHR FAILS - ASCII character returned for blank:', CHAR1
         ISTAT = SAI__ERROR
      ENDIF

      ICHR = A_INVALID
      CHAR1 = CHR_ACHR ( ICHR )
      IF ( CHAR1 .NE. NUL ) THEN
         PRINT *, 
     :   'CHR_ACHR FAILS - ASCII character returned for NUL:', CHAR1
         ISTAT = SAI__ERROR
      ENDIF

      IF (ISTAT .EQ. SAI__OK) THEN
         PRINT *, 'CHR_ACHR OK'
      ELSE
         STATUS = SAI__ERROR
      END IF

      END
