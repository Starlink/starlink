*+  FIT_MEN_OPEN - Open a fit menu file
      SUBROUTINE FIT_MEN_OPEN( GENUS, MFD, STATUS )
*
*    Description :
*
*     Opens a FIT system menu file for read access. The name of the menu file
*     for a fit genus GENUS is stored in an environment variable <GENUS>_MENU
*     on both VMS and UNIX systems.
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Trevor Ponman  (BHVAD::TJP)
*    History :
*
*     14 Sep 92 : Original adapted from FIT_MENU (DJA)
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
      CHARACTER*(*)		GENUS			! Model genus
*
*    Export :
*
      INTEGER            	MFD                    	! AIO input descriptor
*
*    Status :
*
      INTEGER            	STATUS
*
*    Function declarations :
*
      INTEGER            	CHR_LEN
*
*    Local variables :
*
      CHARACTER*132      	MFILE                  ! Menu file name
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

*    Set up AIO for input. Use the AST_MENU_PATH path variable, and
*    a default extension of '.mnu'
      CALL AIO_SETPATH( 'AST_MENU_PATH', STATUS )
      CALL AIO_SETDEXT( '.mnu', STATUS )

*    Access menu file for nested read access
      CALL AIO_FOPEN( MFILE, 'NREAD', MFD, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL MSG_SETC( 'MFILE', MFILE )
        CALL ERR_REP( ' ', 'Menu file ^MFILE not found', STATUS )
      END IF

*    Exit
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'FIT_MEN_OPEN', STATUS )
      END IF

      END
