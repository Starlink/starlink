      SUBROUTINE TEST_CTOL(STATUS)
*+
*  Name:
*     TEST_CTOL

*  Purpose:
*     Test CHR_CTOL.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_CTOL(STATUS)

*  Description:
*     Test CHR_CTOL.
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
*     27-JAN-1997 (AJC):
*        Use EQV/NEQV not EQ/NEQ for logicals
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Subprograms called:   
*     CHR_CTOL

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
      LOGICAL L                  ! LOGICAL value
      LOGICAL L1(4),L2(4)

*.

*    Test CHR_CTOL

      ISTAT = SAI__OK
      CALL CHR_CTOL ('XXX', L, ISTAT)
      IF (ISTAT .NE. SAI__ERROR) THEN
         PRINT *, 'CHR_CTOL FAILS - Error not detected'
         STATUS = SAI__ERROR
      ENDIF
      ISTAT = SAI__OK
      DO I = 1,4
         L1(I) = .FALSE.
         L2(I) = .TRUE.
      ENDDO
      CALL CHR_CTOL ('trUE', L1(1), ISTAT)
      CALL CHR_CTOL ('t', L1(2), ISTAT)
      CALL CHR_CTOL ('yeS', L1(3), ISTAT)
      CALL CHR_CTOL ('y', L1(4), ISTAT)
      CALL CHR_CTOL ('faLSE', L2(1), ISTAT)
      CALL CHR_CTOL ('f', L2(2), ISTAT)
      CALL CHR_CTOL ('nO', L2(3), ISTAT)
      CALL CHR_CTOL ('n', L2(4), ISTAT)
      IF ((ISTAT .EQ. SAI__OK) .AND. 
     :    ((L1(1).AND.L1(2).AND.L1(3).AND.L1(4)) .EQV. .TRUE.) .AND.
     :    .NOT.(L2(1).OR.L2(2).OR.L2(3).OR.L2(4) .EQV. .TRUE.)) THEN
         PRINT *, 'CHR_CTOL OK'
      ELSE
         PRINT *, 'CHR_CTOL FAILS - '
         PRINT *, '''trUE t yeS y'' read as',L1
         PRINT *, '''faLSE f nO n'' read as',L2
         STATUS = SAI__ERROR
      ENDIF

      END
