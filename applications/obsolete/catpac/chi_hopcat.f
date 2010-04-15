      SUBROUTINE CHI_HOPCAT( INPUT, UPDATE_MODE, CATNO, STATUS )

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'
      INCLUDE 'DAT_ERR'
      INCLUDE 'CMP_ERR'
      INCLUDE 'CHI_PAR'          ! CHI constants
      INCLUDE 'CHI_ERR'          ! CHI error codes
      INCLUDE 'CHIH_PAR'         ! CHI_HDS constants
      INCLUDE 'CHIH_ERR'         ! CHI_HDS error codes

*  Status:
      INTEGER STATUS             ! Global status

*  External References:

      CHARACTER*(*)          INPUT
      LOGICAL                UPDATE_MODE
      INTEGER                CATNO      ! Returned Index catalogue number
*  Local Variables:
      INTEGER                MODE
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( UPDATE_MODE ) THEN
          MODE = CHIH__UPDATE
      ELSE
          MODE = CHIH__READ
      ENDIF

      CALL CHI_HGETLOC( INPUT, MODE, CATNO, STATUS )
      IF ( STATUS .NE. SAI__OK) THEN
          STATUS = CHI__CATNOTFND
          GOTO 9999
      ENDIF

9999  RETURN

      END
