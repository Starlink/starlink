      SUBROUTINE
     : CHI_HGNENTS( CATNO, NUMENTS, STATUS )

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard HDS constants
      INCLUDE 'CHI_PAR'          ! Standard CHI constants
      INCLUDE 'CHI_ERR'          ! Standard CHI errors
      INCLUDE 'CHIH_PAR'        ! CHI_HDS constants
      INCLUDE 'DAT_ERR'
      INCLUDE 'CMP_ERR'
      INCLUDE 'CHIH_ERR'         ! CHI_HDS error codes

*  Arguments Given:
      INTEGER           CATNO
      INTEGER           NUMENTS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      NUMENTS = 0

      CALL CHI_HCHECKLOC( CATNO, 0, STATUS)
      IF ( STATUS .NE. SAI__OK) THEN
          STATUS = CHI__CATNOTFND
          GOTO 9999
      ENDIF

      NUMENTS = CHIH_TOTUSED( 1, CATNO)

9999  RETURN
      END

