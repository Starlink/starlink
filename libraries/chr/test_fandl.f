      SUBROUTINE TEST_FANDL(STATUS)
*+
*  Name:
*     TEST_FANDL

*  Purpose:
*     Test CHR_FANDL.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_FANDL(STATUS)

*  Description:
*     Test CHR_FANDL.
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
*     CHR_FANDL

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
      INTEGER PTR1, PTR2         ! String indexes
      CHARACTER*120 STRING

*.

*    Test CHR_FANDL

*    First an empty string
      CALL CHR_FANDL( '  ', PTR1, PTR2 )
      IF ( .NOT.((PTR1 .EQ. 2) .AND. (PTR2 .EQ. 0))) THEN
         PRINT *, 'CHR_FANDL FAILS - two spaces returns', PTR1, PTR2
         STATUS = SAI__ERROR
      ENDIF

*    Now a normal string including internal spaces
      STRING = '   A B  C   '
      CALL CHR_FANDL( STRING, PTR1, PTR2 )
      IF ( (PTR1 .EQ. 4) .AND. (PTR2 .EQ. 9)) THEN
         PRINT *, 'CHR_FANDL OK'
      ELSE
         PRINT *, 'CHR_FANDL FAILS - STRING is:',STRING,' POINTERS are',
     :             PTR1, PTR2
         STATUS = SAI__ERROR
      ENDIF

      END
