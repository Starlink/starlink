*+  AIO_WRITE - Write text to an output channel
      SUBROUTINE AIO_WRITE( ID, TEXT, STATUS )
*
*    Description :
*
*     Writes text to output channel
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
*     Richard Beard (University of Birmingham)
*
*    History :
*
*      4 May 94 : Original. Derived from old UTIL_SELOUT routine (DJA)
*      3 Apr 98 : Try to get 132 width screens (RB)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'AIO_PAR'
*
*    Global variables :
*
      INCLUDE 'AIO_CMN'
*
*    Import :
*
      INTEGER			ID			! Channel id
      CHARACTER*(*)		TEXT			! Text to output
*
*    Status :
*
      INTEGER 			STATUS
*
*    External references :
*
      EXTERNAL                  AIO_BLK
*
*    Local variables :
*
      CHARACTER*256		BUF			! Output buffer

      INTEGER			BLEN			! Buffer length
      INTEGER                   LID                     ! Local channel id
      INTEGER                   LWID                    ! Local channel width
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    If AIO isn't up and running, write to terminal
      IF ( .NOT. AIO_DEF ) THEN
        LID = AIO__M_CONSOLE
        LWID = 132
      ELSE
        LID = ID
        LWID = AIO_WIDTH
      END IF

*    Translate any tokens
      CALL MSG_LOAD( ' ', TEXT, BUF, BLEN, STATUS )

*    Switch on mode to write the text
      IF ( LID .EQ. AIO__M_CONSOLE ) THEN
        WRITE( 6, '(A)' ) BUF(:BLEN)
      ELSE
        CALL FIO_WRITE( AIO_FID, BUF(:BLEN), STATUS )
      END IF

*    Tidy up
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'AIO_WRITE', STATUS )
      END IF

      END
