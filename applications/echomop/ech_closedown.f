      SUBROUTINE ECH_CLOSEDOWN( STATUS )
*+
*  Name:
*     ECHOMOP - ECH_CLOSEDOWN

*  Purpose:
*     Call environment closedown routines.

*  Invocation:
*     CALL ECH_CLOSEDOWN( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     De-access main ECHOMOP parameters
*     Closedown environment
*     If sucessful then
*        Reset global open-files flags
*     Endif

*  Bugs:
*     None known.

*  Authors:
*     Dave Mills STARLINK (ZUVAD::DMILLS)

*  History:
*     1992 Sept 1: Initial release

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_USE_RDCTN.INC'

*  Status:
      INTEGER STATUS

*  Local variables:
      INTEGER DUMDIM( MAX_DIMENSIONS )
*.

*  De-access main ECHOMOP parameters.
      CALL ECH_MODULE_TIDYUP( 'ECHOMOP', STATUS )

*  Closedown environment.
      CALL ECH_ACCESS_OBJECT( ' ' , 'CLOSE' , 'ENVIRONMENT' ,
     :     0 , 0 , 0 , DUMDIM, MAX_DIMENSIONS, 0 , ' ' , STATUS )
      IF ( STATUS .EQ. 0 ) THEN

*     Reset global open-files flags
          RDCTN_FILE_OPEN = .FALSE.
          DATA_FILES_OPEN = .FALSE.
          NUM_REG_VARS = 0
          ACCESS_COUNT = 0
      END IF

      END
