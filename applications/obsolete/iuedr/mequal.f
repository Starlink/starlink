      SUBROUTINE MEQUAL( NBAD, S_VM, L_VM, Q_VM, STATUS )

*+
*
*   Name:
*      SUBROUTINE MEQUAL
*
*   Description:
*      Get VM for sparse data quality array (list).
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          22-SEP-88     IUEDR Vn. 2.0
*      Martin Clayton     30-SEP-94     IUEDR Vn. 3.1-6
*
*   Method:
*
*-

*   Implicit:
      IMPLICIT NONE

*   Starlink includes:
      INCLUDE 'SAE_PAR'

*   Import:
      INTEGER NBAD ! number of subset pixels

*   Export:
      INTEGER S_VM       ! address of SBAD
      INTEGER L_VM       ! address of LBAD
      INTEGER Q_VM       ! address of QBAD
      INTEGER STATUS     ! status return

*   Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Release existing S_VM
      IF ( S_VM .GT. 0 ) CALL DLADR( S_VM, STATUS )

*   Get new S_VM
      CALL ALADR( 'int\\', NBAD, S_VM, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: getting VM\\', STATUS )
         RETURN
      END IF

*   Release existing L_VM
      IF ( L_VM .GT. 0 ) CALL DLADR( L_VM, STATUS )

*   Get new L_VM
      CALL ALADR( 'int\\', NBAD, L_VM, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: getting VM\\', STATUS )
         RETURN
      END IF

*   Release existing Q_VM
      IF ( Q_VM .GT. 0 ) CALL DLADR( Q_VM, STATUS )

*   Get new Q_VM
      CALL ALADR( 'byte\\', NBAD, Q_VM, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: getting VM\\', STATUS )
         RETURN
      END IF
      END
