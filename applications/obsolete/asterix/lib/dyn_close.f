*+  DYN_CLOSE - close down DYN_ system
      SUBROUTINE DYN_CLOSE()
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*
*     29 Apr 92 : Use ERR_ to provide error protection. Removed unnecessary
*                 HDS_CLOSE calls (DJA)
*     16 Jun 92 : Replaced LIB$ calls with PSX ones (DJA)
*
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Status :
      INTEGER STATUS
*    Import :
*    Import/Export :
*    Export :
*    External references :
*    Global variables :
      INCLUDE 'ASTLIB(DYN_CMN)'
*    Local Constants :
      INTEGER NULL
      PARAMETER (NULL=0)
*    Local variables :
      INTEGER IPTR
*-

*    Start new error context
      CALL ERR_BEGIN( STATUS )

      DO IPTR=1,NPTR
        IF (LIST(IPTR).SECTION) THEN
          CALL HDS_ERASE(LIST(IPTR).LOC,STATUS)
          IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
          END IF
        ELSE
          CALL PSX_FREE( LIST(IPTR).PTR, STATUS )
        ENDIF
        LIST(IPTR).PTR=NULL
        LIST(IPTR).NPAGE=NULL
        LIST(IPTR).NBYTE=NULL
        LIST(IPTR).SECTION=.FALSE.
      ENDDO
      NPTR=NULL

*    Restore error context
      CALL ERR_END( STATUS )

      END
