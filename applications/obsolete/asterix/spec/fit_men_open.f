*+  FIT_MEN_OPEN - Open a fit menu file
      SUBROUTINE FIT_MEN_OPEN( GENUS, MFD, STATUS )
*
*    Description :
*
*     Opens a FIT system menu file for read access. The name of the menu file
*     for a fit genus GENUS is stored in an environment variable <GENUS>_MENU
*     on both VMS and ULTRIX systems.
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Trevor Ponman  (BHVAD::TJP)
*    History :
*
*     14 Sep 92 : Original adapted from FIT_MENU (DJA)
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
      CHARACTER*4        GENUS			! Model genus
*
*    Export :
*
      INTEGER            MFD                    ! FIO menu file descriptor
*
*    Status :
*
      INTEGER            STATUS
*
*    Function declarations :
*
      INTEGER            CHR_LEN
*
*    Local variables :
*
      CHARACTER*132      MFILE                  ! Menu file name
*-

*    Status check
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Translate environment variable <GENUS>_MENU
      CALL PSX_GETENV( GENUS(:CHR_LEN(GENUS))//'_MENU', MFILE, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ', 'Unable to get environment variable '/
     :                                  /GENUS//'_MENU', STATUS )
        GOTO 99
      END IF

*    Access menu file
      CALL FIO_OPEN( MFILE, 'READ', 'LIST', 0, MFD, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL MSG_SETC( 'MFILE', MFILE )
        CALL ERR_REP( ' ', 'Menu file ^MFILE not found', STATUS )
        GOTO 99
      END IF

*    Exit
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ', '...from FIT_MEN_OPEN', STATUS )
      END IF

      END
