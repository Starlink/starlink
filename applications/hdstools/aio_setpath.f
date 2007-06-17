*+  AIO_SETPATH - Set AIO input path variable
      SUBROUTINE AIO_SETPATH( PATH, STATUS )
*
*    Description :
*
*     Sets AIO input PATH variable. This is used when opening existing files
*     for read access.
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
      CHARACTER*(*)		PATH			! New path value
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

*    Set path variable
      AIO_IPATH = PATH

      END
