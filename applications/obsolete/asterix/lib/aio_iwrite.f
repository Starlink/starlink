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
*      4 May 94 : Original. Derived from old UTIL_SELOUT routine (DJA)
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
      CHARACTER*80		BLANKS
        DATA			BLANKS/'                             '/
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Write the text with leading blanks
      CALL AIO_WRITE( ID, BLANKS(1:INDENT)//TEXT, STATUS )

*    Tidy up
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'AIO_IWRITE', STATUS )
      END IF

      END
