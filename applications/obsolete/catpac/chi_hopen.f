      SUBROUTINE CHI_HOPEN( STATUS )

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

*  Local Variables:
      INTEGER  I, J
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*** COMMENT NEXT LINE OUT IF INSIDE_ADAM_ENVIRONMENT
*************       CALL HDS_START( STATUS )

      DO I = 1, CHIH__NUMCATS
          DO J = 1, CHIH__NUMASS
              CHIH_CATNAME(   J, I) = ' '
              CHIH_LOC(       J, I) = ' '
              CHIH_MODE(      J, I) = 0
              CHIH_LASTRECACC(J, I) = 0
              CHIH_TOTUSED(   J, I) = 0
              CHIH_TOTSIZE(   J, I) = 0
          ENDDO
          CHIH_NUMASSOC(I)      = 0
          CHIH_MAPSEC(1, I)     = 0
          CHIH_MAPSEC(2, I)     = 0
      ENDDO

      CHIH_MODECODE(1) = 'READ'
      CHIH_MODECODE(2) = 'WRITE'
      CHIH_MODECODE(3) = 'UPDATE'

      CURR_CAT   = 0
      NCATS_OPEN = 0

      END

