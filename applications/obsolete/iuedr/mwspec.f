      SUBROUTINE MWSPEC( NAXIS1, NAXIS2, D_VM, Q_VM, STATUS )
*+
*  Name:
*     SUBROUTINE MWSPEC

*  Purpose:
*     Release allocated memory and re-allocate shorts for data only.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MWSPEC( NAXIS1, NAXIS2, D_VM, Q_VM, STATUS )

*  Arguments:
*     NAXIS1 = INTEGER (Given)
*        X-axis size of array.
*     NAXIS2 = INTEGER (Given)
*        Y-axis size of array.
*     D_VM = INTEGER (Given and Returned)
*        Pointer to Data memory.
*     Q_VM = INTEGER (Given and Returned)
*        Pointer to Quality information memory.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     22-SEP-88 (PCTR):
*       IUEDR Vn. 2.0
*     20-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER NAXIS1     ! Axis1 size.
      INTEGER NAXIS2     ! Axis2 size.

*  Arguments Given and Returned:
      INTEGER D_VM       ! DATA VM.
      INTEGER Q_VM       ! QUAL VM.

*  Status:
      INTEGER STATUS     ! Global status.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Release existing DATA_VM.
      IF ( D_VM .GT. 0 ) THEN
         CALL DLADR( D_VM, STATUS )
      END IF

*   Release existing QUAL_VM.
      IF ( Q_VM .GT. 0 ) THEN
         CALL DLADR( Q_VM, STATUS )
      END IF

*   Get new DATA_VM (only).
      CALL ALADR( 'short\\', NAXIS1 * NAXIS2, D_VM, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: getting DATA VM\\', STATUS )
         GO TO 999
      END IF

 999  CONTINUE

      END
