*+  AIO_SETDEXT - Set AIO default input extension
      SUBROUTINE AIO_SETDEXT( DEXT, STATUS )
*
*    Description :
*
*     Sets AIO input DEXT variable. This is used when opening existing files
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
      INCLUDE 'ASTLIB(AIO_CMN)'
*
*    Import :
*
      CHARACTER*(*)		DEXT			! New default extension
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

*    Set default extension
      AIO_DEFEXT = DEXT

      END
