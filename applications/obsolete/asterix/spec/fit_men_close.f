*+  FIT_MEN_CLOSE - Close a fit menu file
      SUBROUTINE FIT_MEN_CLOSE( MFD, STATUS )
*
*    Description :
*
*     Closes a fit menu file.
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Trevor Ponman  (BHVAD::TJP)
*    History :
*
*     15 Sep 92 : Original adapted from FIT_MENU (DJA)
*     11 Nov 94 : Use AIO for input (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      INTEGER            MFD                    ! AIO menu file descriptor
*
*    Status :
*
      INTEGER            STATUS
*-

*    Status check
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Close file
      CALL AIO_CLOSE( MFD, STATUS )

*    Flush errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ', 'Error closing menu file', STATUS )
        CALL ERR_FLUSH( STATUS )
      END IF

      END
