      SUBROUTINE SGS_TEST(STATUS)
*-
*   Test SGS environment level.
*-
      INCLUDE 'SAE_PAR'
      INCLUDE 'SGS_ERR'

      INTEGER STATUS
      INTEGER IZONID,IZ

      CALL SGS_ASSOC( 'DEVICE', 'WRITE', IZONID, STATUS)
      IF (STATUS .NE. SAI__OK) RETURN

*  Declare a shaped zone on the first device
      CALL SGS_ZSHAP(1.0, 'O', IZ, STATUS)
      IF (STATUS .NE. SAI__OK) RETURN

*  BOX
      CALL SGS_BOX( 0.1, 0.9, 0.4, 0.6)

*  MESSAGE
      CALL SGS_SHTX( 0.1 )
      CALL SGS_STXJ( 'CC' )
      CALL SGS_BTEXT( 0.5, 0.5)
      CALL SGS_ATEXT('STARLINK')

      CALL SGS_CANCL( 'DEVICE', STATUS )
      CALL SGS_DEACT(  STATUS )

      END
