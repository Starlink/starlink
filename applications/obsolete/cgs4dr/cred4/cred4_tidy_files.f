*+  CRED4_TIDY_FILES - Deletes $CGS4_ENG/emlt.lis, spectrum%.*
      SUBROUTINE CRED4_TIDY_FILES( STATUS )
*    Authors :
*     P N Daly (JACH.HAWAII.EDU::PND)
*    History :
*     19-Jan-1990: Original Unix version           (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'CRED4COM.INC'
*    Status :
      INTEGER STATUS     ! Global status
*    External references :
      INTEGER CHR_LEN
*    Local variables :
      CHARACTER*(MSG_VAL_LEN) EFILE
      CHARACTER*(MSG_VAL_LEN) S1FILE
      CHARACTER*(MSG_VAL_LEN) S2FILE
*-

*    Check for error on entry
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Set the filenames
      CALL CHR_FILL( ' ', EFILE )
      EFILE = PREFIX // 'CGS4_ENG' // SEPARATOR // 'emlt.lis'
      CALL CHR_RMBLK( EFILE )
      CALL CHR_FILL( ' ', S1FILE )
      S1FILE = PREFIX // 'CGS4_DATA' // SEPARATOR // 'spectrum1'
      CALL CHR_RMBLK( S1FILE )
      CALL CHR_FILL( ' ', S2FILE )
      S2FILE = PREFIX // 'CGS4_DATA' // SEPARATOR // 'spectrum2'
      CALL CHR_RMBLK( S2FILE )

*    Delete all copies of $CGS4_ENG/emlt.lis until not found
      DO WHILE ( STATUS .EQ. SAI__OK )
        CALL FIO_ERASE( EFILE(1:CHR_LEN(EFILE)), STATUS )
      ENDDO

*    Delete all copies of $CGS4_DATA/spectrum1.dst  or $CGS4_DATA/spectrum1.sdf until not found
      CALL ERR_ANNUL( STATUS )
      DO WHILE ( STATUS .EQ. SAI__OK )
        CALL FIO_ERASE( S1FILE(1:CHR_LEN(S1FILE))//'.dst', STATUS )
      ENDDO
      CALL ERR_ANNUL( STATUS )
      DO WHILE ( STATUS .EQ. SAI__OK )
        CALL FIO_ERASE( S1FILE(1:CHR_LEN(S1FILE))//'.sdf', STATUS )
      ENDDO

*    Delete all copies of $CGS4_DATA/spectrum2.dst  or $CGS4_DATA/spectrum2.sdf until not found
      CALL ERR_ANNUL( STATUS )
      DO WHILE ( STATUS .EQ. SAI__OK )
        CALL FIO_ERASE( S2FILE(1:CHR_LEN(S2FILE))//'.dst', STATUS )
      ENDDO
      CALL ERR_ANNUL( STATUS )
      DO WHILE ( STATUS .EQ. SAI__OK )
        CALL FIO_ERASE( S2FILE(1:CHR_LEN(S2FILE))//'.sdf', STATUS )
      ENDDO
      CALL ERR_ANNUL( STATUS )

      END
