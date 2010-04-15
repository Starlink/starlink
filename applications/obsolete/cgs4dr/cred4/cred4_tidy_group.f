*+  CRED4_TIDY_GROUP - Deletes temporary files associated with a group.
      SUBROUTINE CRED4_TIDY_GROUP( GROUP, STATUS )
*    Invocation :
*     CALL CRED4_TIDY_GROUP( GROUP, STATUS )
*    Parameters :
*    Authors :
*     P N Daly   (JACH.HAWAII.EDU::PND)
*    History :
*     19-Jan-1995: Original Unix version  (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      CHARACTER*(*) GROUP        ! Group file name.
*    Status :
      INTEGER STATUS             ! Global status
*    External references :
      INTEGER CHR_LEN            ! Character length determining function
*    Global variables :
      INCLUDE 'CRED4COM.INC'     ! CRED4 common block.
*    Local variables :
      CHARACTER*255 TMP_GROUP    ! Name of temporary files
      INTEGER CLEN
*-

*    Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get the full group name
      CLEN = INDEX( GROUP, SEPARATOR )
      TMP_GROUP = RGDIR(1:CHR_LEN(RGDIR)) // GROUP(CLEN+1:CHR_LEN(GROUP))

*    Delete _pf files
      CALL ERR_ANNUL( STATUS )
      DO WHILE ( STATUS .EQ. SAI__OK )
        CALL FIO_ERASE( TMP_GROUP(1:CHR_LEN(TMP_GROUP))//'_pf.sdf', STATUS )
      ENDDO
      CALL ERR_ANNUL( STATUS )
      DO WHILE ( STATUS .EQ. SAI__OK )
        CALL FIO_ERASE( TMP_GROUP(1:CHR_LEN(TMP_GROUP))//'_pf.dst', STATUS )
      ENDDO

*    Delete _dbs files
      CALL ERR_ANNUL( STATUS )
      DO WHILE ( STATUS .EQ. SAI__OK )
        CALL FIO_ERASE( TMP_GROUP(1:CHR_LEN(TMP_GROUP))//'_dbs.sdf', STATUS )
      ENDDO
      CALL ERR_ANNUL( STATUS )
      DO WHILE ( STATUS .EQ. SAI__OK )
        CALL FIO_ERASE( TMP_GROUP(1:CHR_LEN(TMP_GROUP))//'_dbs.dst', STATUS )
      ENDDO

*    Delete _spc files
      CALL ERR_ANNUL( STATUS )
      DO WHILE ( STATUS .EQ. SAI__OK )
        CALL FIO_ERASE( TMP_GROUP(1:CHR_LEN(TMP_GROUP))//'_spc.sdf', STATUS )
      ENDDO
      CALL ERR_ANNUL( STATUS )
      DO WHILE ( STATUS .EQ. SAI__OK )
        CALL FIO_ERASE( TMP_GROUP(1:CHR_LEN(TMP_GROUP))//'_spc.dst', STATUS )
      ENDDO

*    Delete _imspc files
      CALL ERR_ANNUL( STATUS )
      DO WHILE ( STATUS .EQ. SAI__OK )
        CALL FIO_ERASE( TMP_GROUP(1:CHR_LEN(TMP_GROUP))//'_imspc.sdf', STATUS )
      ENDDO
      CALL ERR_ANNUL( STATUS )
      DO WHILE ( STATUS .EQ. SAI__OK )
        CALL FIO_ERASE( TMP_GROUP(1:CHR_LEN(TMP_GROUP))//'_imspc.dst', STATUS )
      ENDDO

*    Delete _pf_dbs files
      CALL ERR_ANNUL( STATUS )
      DO WHILE ( STATUS .EQ. SAI__OK )
        CALL FIO_ERASE( TMP_GROUP(1:CHR_LEN(TMP_GROUP))//'_pf_dbs.sdf', STATUS )
      ENDDO
      CALL ERR_ANNUL( STATUS )
      DO WHILE ( STATUS .EQ. SAI__OK )
        CALL FIO_ERASE( TMP_GROUP(1:CHR_LEN(TMP_GROUP))//'_pf_dbs.dst', STATUS )
      ENDDO

*    Delete _pf_spc files
      CALL ERR_ANNUL( STATUS )
      DO WHILE ( STATUS .EQ. SAI__OK )
        CALL FIO_ERASE( TMP_GROUP(1:CHR_LEN(TMP_GROUP))//'_pf_spc.sdf', STATUS )
      ENDDO
      CALL ERR_ANNUL( STATUS )
      DO WHILE ( STATUS .EQ. SAI__OK )
        CALL FIO_ERASE( TMP_GROUP(1:CHR_LEN(TMP_GROUP))//'_pf_spc.dst', STATUS )
      ENDDO

*    Delete _pf_imspc files
      CALL ERR_ANNUL( STATUS )
      DO WHILE ( STATUS .EQ. SAI__OK )
        CALL FIO_ERASE( TMP_GROUP(1:CHR_LEN(TMP_GROUP))//'_pf_imspc.sdf', STATUS )
      ENDDO
      CALL ERR_ANNUL( STATUS )
      DO WHILE ( STATUS .EQ. SAI__OK )
        CALL FIO_ERASE( TMP_GROUP(1:CHR_LEN(TMP_GROUP))//'_pf_imspc.dst', STATUS )
      ENDDO

*    Delete _dbs_spc files
      CALL ERR_ANNUL( STATUS )
      DO WHILE ( STATUS .EQ. SAI__OK )
        CALL FIO_ERASE( TMP_GROUP(1:CHR_LEN(TMP_GROUP))//'_dbs_spc.sdf', STATUS )
      ENDDO
      CALL ERR_ANNUL( STATUS )
      DO WHILE ( STATUS .EQ. SAI__OK )
        CALL FIO_ERASE( TMP_GROUP(1:CHR_LEN(TMP_GROUP))//'_dbs_spc.dst', STATUS )
      ENDDO

*    Delete _dbs_imspc files
      CALL ERR_ANNUL( STATUS )
      DO WHILE ( STATUS .EQ. SAI__OK )
        CALL FIO_ERASE( TMP_GROUP(1:CHR_LEN(TMP_GROUP))//'_dbs_imspc.sdf', STATUS )
      ENDDO
      CALL ERR_ANNUL( STATUS )
      DO WHILE ( STATUS .EQ. SAI__OK )
        CALL FIO_ERASE( TMP_GROUP(1:CHR_LEN(TMP_GROUP))//'_dbs_imspc.dst', STATUS )
      ENDDO

*    Delete _pf_dbs_spc files
      CALL ERR_ANNUL( STATUS )
      DO WHILE ( STATUS .EQ. SAI__OK )
        CALL FIO_ERASE( TMP_GROUP(1:CHR_LEN(TMP_GROUP))//'_pf_dbs_spc.sdf', STATUS )
      ENDDO
      CALL ERR_ANNUL( STATUS )
      DO WHILE ( STATUS .EQ. SAI__OK )
        CALL FIO_ERASE( TMP_GROUP(1:CHR_LEN(TMP_GROUP))//'_pf_dbs_spc.dst', STATUS )
      ENDDO

*    Delete _pf_dbs_imspc files
      CALL ERR_ANNUL( STATUS )
      DO WHILE ( STATUS .EQ. SAI__OK )
        CALL FIO_ERASE( TMP_GROUP(1:CHR_LEN(TMP_GROUP))//'_pf_dbs_imspc.sdf', STATUS )
      ENDDO
      CALL ERR_ANNUL( STATUS )
      DO WHILE ( STATUS .EQ. SAI__OK )
        CALL FIO_ERASE( TMP_GROUP(1:CHR_LEN(TMP_GROUP))//'_pf_dbs_imspc.dst', STATUS )
      ENDDO
      CALL ERR_ANNUL( STATUS )
      END
