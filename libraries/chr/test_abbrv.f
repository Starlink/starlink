      SUBROUTINE TEST_ABBRV(STATUS)
*+
*  Name:
*     TEST_ABBRV

*  Purpose:
*     Test CHR_ABBRV.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_ABBRV(STATUS)

*  Description:
*     Test CHR_ABBRV.
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
*     CHR_ABBRV

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
      LOGICAL CHR_ABBRV

*  Local Variables:
      INTEGER JSTAT              ! Local status
      LOGICAL LSTAT

*.

*    Test CHR_ABBRV

*    Equal strings, NCHAR too big
      LSTAT = .TRUE.
      JSTAT = SAI__OK
      LSTAT = CHR_ABBRV ('ABCDEF','ABCDEF',7)
      IF ((.NOT. LSTAT)) THEN
         PRINT *, 'CHR_ABBRV equal strings FAILS'
         JSTAT = SAI__ERROR
      ENDIF

*    Abbreviated string
      LSTAT = CHR_ABBRV ('ABC','ABCDEF',3)
      IF ((.NOT. LSTAT)) THEN
         PRINT *, 'CHR_ABBRV abbreviated string FAILS'
         JSTAT = SAI__ERROR
      ENDIF

*    Invalid Abbreviation
      LSTAT = CHR_ABBRV ('ABCF','ABCDEF',3)
      IF (( LSTAT)) THEN
         PRINT *, 'CHR_ABBRV invalid match detection FAILS'
         JSTAT = SAI__ERROR
      ENDIF

      IF (JSTAT .EQ. SAI__OK) THEN
         PRINT *, 'CHR_ABBRV OK'
         STATUS = JSTAT
      END IF

      END
