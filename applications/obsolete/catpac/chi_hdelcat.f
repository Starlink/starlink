      SUBROUTINE
     : CHI_HDELCAT( INPUT, STATUS)

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

      CHARACTER * ( * ) INPUT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:

      INTEGER CATNO
      CHARACTER*(DAT__SZLOC) TBDSCR
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL TBL_OPEN( INPUT, 'UPDATE', TBDSCR, STATUS)
      CALL TBL_DELTAB( TBDSCR, STATUS)

      IF ( STATUS .NE. SAI__OK) THEN
          STATUS = CHI__CATNOTFND
          CALL ERR_REP(' ', 'HDS error deleting table', STATUS)
      ENDIF

9999  RETURN

      END
