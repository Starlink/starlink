*+  AIO_ASSOCO - Open an Asterix text output channel selected by ADAM parameter
      SUBROUTINE AIO_ASSOCO( PAR, TYPE, ID, WIDTH, STATUS )
*
*    Description :
*
*     Opens the character device associated with the user's response to the
*     named ADAM parameter. See AIO_OPEN for a list of valid responses.
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
*      4 May 94 : Original (DJA)
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
      INCLUDE 'ASTLIB(AIO_CMN)'
*
*    Import :
*
      CHARACTER*(*)		PAR			! ADAM parameter name
      CHARACTER*(*)		TYPE			! Type of file
*
*    Export :
*
      INTEGER			ID			! Channel id
      INTEGER			WIDTH			! Width in characters
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*80		VALUE			! User response
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get response
      CALL PAR_GET0C( PAR, VALUE, STATUS )

*    Open device
      CALL AIO_OPEN( VALUE, TYPE, ID, WIDTH, STATUS )

*    Store parameter
      AIO_PARNAM = PAR

*    Tidy up
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'AIO_ASSOCO', STATUS )
      END IF

      END
