*+  AIO_CLOSE - Close Asterix output text channel
      SUBROUTINE AIO_CLOSE( ID, STATUS )
*
*    Description :
*
*     Closes output text channel. If the channel maps to a file then the
*     file is closed. If the channel maps to the printer the file is
*     spooled and deleted.
*
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*
*     David J. Allan (JET-X, University of Birmingham)
*
*    History :
*
*      4 May 94 : Original. Derived from old UTIL_SELOUT routine (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'AIO_PAR'
      INCLUDE 'PAR_PAR'
*
*    Global variables :
*
      INCLUDE 'ASTLIB(AIO_CMN)'
*
*    Import :
*
      INTEGER			ID			! Channel id
*
*    Status :
*
      INTEGER 			STATUS
*
*    Function declarations :
*
      INTEGER			CHR_LEN
*
*    External references :
*
      EXTERNAL              	AIO_BLK
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    If not console mode
      IF ( ID .NE. AIO__M_CONSOLE ) THEN

*      Close the file
        CALL FIO_CLOSE( AIO_FID, STATUS )

*      Annul any errors
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          CALL MSG_OUT( ' ', 'Warning : Error closing output text file',
     :                  STATUS )
        END IF

*      Spool and delete if the mode is print
        IF ( ID .EQ. AIO__M_PRINT ) THEN
          CALL UTIL_SPOOL( AIO_FILE, AIO_TYPE(:CHR_LEN(AIO_TYPE)),
     :                     .TRUE., STATUS )
          IF ( STATUS .EQ. SAI__OK ) THEN
            CALL MSG_OUT( ' ', 'Output file spooled to printer',
     :                    STATUS )
          END IF
        END IF

      END IF

*    Not defined now
      AIO_DEF = .FALSE.

*    Tidy up
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'AIO_CLOSE', STATUS )
      END IF

      END
