      SUBROUTINE ECH_CREATE_REDUCTION_FILE( FILETYPE, STATUS )
*+
*  Name:
*     ECHOMOP - ECH_CREATE_REDUCTION_FILE

*  Purpose:
*     Use the environment-specific routines necessary to create container file.

*  Description:
*     This routine uses the environment specific file creation routines to
*     create a container file to hold all the objects created during a
*     reduction session.

*  Invocation:
*     CALL ECH_CREATE_REDUCTION_FILE( FILETYPE, STATUS )

*  Arguments:
*     FILETYPE = (Given)
*        File name type (for structure definition).
*     STATUS = (Given and Returned)
*        Input/Output status conditions.

*  Bugs:
*     None known.

*  Authors:
*     Dave Mills STARLINK (ZUVAD::DMILLS)

*  History:
*     1992 Sept 1 : Initial release

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_USE_RDCTN.INC'
      INCLUDE 'ECH_REPORT.INC'

*  Arguments Given:
      CHARACTER*( * ) FILETYPE

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER DUMDIM( MAX_DIMENSIONS )

*  Functions Called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Initialise environment system.
      STATUS = 0
      IF ( .NOT. DATA_FILES_OPEN )
     :   CALL ECH_ACCESS_OBJECT( ' ', 'OPEN', 'ENVIRONMENT',
     :        0, 0, 0, DUMDIM, MAX_DIMENSIONS, 0, ' ', STATUS )
      CALL ECH_ACCESS_OBJECT( FILETYPE, 'CREATE', 'REDUCTION', 0,
     :     0, 0, DUMDIM, MAX_DIMENSIONS, 0, RDCTN_FILE_NAME, STATUS )

      END
