*+  AIO_SETWRAP - Set internal line wrap state
      SUBROUTINE AIO_SETWRAP( STATE, STATUS )
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
*      9 May 94 : Original. (DJA)
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
      LOGICAL			STATE			! New state of wrap
*
*    Status :
*
      INTEGER 			STATUS
*
*    External references :
*
      EXTERNAL                  AIO_BLK
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Set state variable
      AIO_WRAP = STATE

      END
