      SUBROUTINE CHI_HCHECKLOC( CATNO, MODE, STATUS )

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

      INTEGER                CATNO      ! Index catalogue number
      INTEGER                MODE       ! Mode of access wanted

*  Local Variables:

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN


      IF ( CATNO .LE. 0 .OR. CATNO. GT. NCATS_OPEN) THEN
          STATUS = CHI__CATNOTFND
          CALL ERR_REP( ' ',
     : 'Catalogue descriptor illegal - neg or too large', STATUS)
      ELSE
          IF ( CHIH_NUMASSOC(CATNO) .GT. 0 .AND.
     :         CHIH_MODE(1, CATNO) .GE. MODE     )  THEN
              GOTO 9999
          ELSE
              STATUS = CHI__CATNOTFND
              CALL ERR_REP( ' ',
     : 'Catalogue descriptor incorrect or access mode mismatch', STATUS)
          ENDIF
      ENDIF

9999  RETURN

      END
