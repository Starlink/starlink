      SUBROUTINE TEST_ATOK(STATUS)
*+
*  Name:
*     TEST_ATOK

*  Purpose:
*     Test CHR_ATOK.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_ATOK(STATUS)

*  Description:
*     Test CHR_ATOK.
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
*     CHR_ATOK

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
      CHARACTER*1 CHR_ATOK
      INTEGER CHR_IACHR

*  Local Constants:
      INTEGER A_BEL
      PARAMETER (A_BEL = 7)
      INTEGER A_LF
      PARAMETER (A_LF = 10)
      INTEGER A_BS
      PARAMETER (A_BS = 8)

*  Local Variables:
      INTEGER ISTAT              ! Local status
      INTEGER ICHR               ! INTEGER value

*  Local Data:

*.

*    Test CHR_ATOK

      ISTAT = SAI__OK
      ICHR = CHR_IACHR( CHR_ATOK( 'BEL' ) )
      IF ( ICHR .NE. A_BEL ) THEN
         PRINT *, 'CHR_ATOK FAILS - numeric value returned for BEL:', 
     :            ICHR
         ISTAT = SAI__ERROR
      ENDIF

      ICHR = CHR_IACHR( CHR_ATOK( 'LF' ) )
      IF ( ICHR .NE. A_LF ) THEN
         PRINT *, 'CHR_ATOK FAILS - numeric value returned for LF:', 
     :            ICHR
         ISTAT = SAI__ERROR
      ENDIF

      ICHR = CHR_IACHR( CHR_ATOK( 'BS' ) )
      IF ( ICHR .NE. A_BS ) THEN
         PRINT *, 'CHR_ATOK FAILS - numeric value returned for BS:', 
     :            ICHR
         ISTAT = SAI__ERROR
      ENDIF

      IF (ISTAT .EQ. SAI__OK) THEN
         PRINT *, 'CHR_ATOK OK'
      ELSE
         STATUS = SAI__ERROR
      END IF

      END
