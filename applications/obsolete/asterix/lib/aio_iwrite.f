*+  AIO_IWRITE - Write text to an output channel with indent
      SUBROUTINE AIO_IWRITE( ID, INDENT, TEXT, STATUS )
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
*
*    History :
*
*      4 May 1994 (DJA):
*        Original version. Derived from old UTIL_SELOUT routine
*     22 Feb 1996 (DJA):
*        Strict F77 version to cope with Linux
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      INTEGER			ID			! Channel id
      INTEGER			INDENT			! Indentation
      CHARACTER*(*)		TEXT			! Text to output
*
*    Status :
*
      INTEGER 			STATUS
*
*    Local data :
*
      CHARACTER*200		OBUF			! Output buffer
      CHARACTER*80		BLANKS
        DATA			BLANKS/'                             '/
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write blanks to buffer
      IF ( INDENT .GT. 0 ) THEN
        OBUF(:INDENT) = BLANKS(:INDENT)
      END IF

*  Fill rest of buffer
      OBUF(INDENT+1:) = TEXT

*  Write buffer
      CALL AIO_WRITE( ID, OBUF(:INDENT+LEN(TEXT)), STATUS )

*  Tidy up
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'AIO_IWRITE', STATUS )
      END IF

      END
