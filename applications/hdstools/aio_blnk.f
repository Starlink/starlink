*+  AIO_BLNK - Write a blank line to an output channel
      SUBROUTINE AIO_BLNK( ID, STATUS )
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
*
*    Status :
*
      INTEGER 			STATUS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Write a blank line
      CALL AIO_WRITE( ID, ' ', STATUS )

*    Tidy up
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'AIO_BLNK', STATUS )
      END IF

      END
