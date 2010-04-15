      SUBROUTINE POL1_CNFIG( FILE, STATUS )
*+
*  Name:
*     POL1_CNFIG

*  Purpose:
*     Reads the polpack configuration file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_CNFIG( FILE, STATUS )

*  Description:
*     This routine reads the polpack configuration file. If FILE is not
*     blank configuration ptions are read from there. Otherwise, they are
*     read from the fil epointed to by env. variable POLPACKRC. Otherwise
*     they are read form $HOME/.polpackrc.

*  Arguments:
*     FILE = CHARACTER * ( * ) (Given)
*        The config file, or blank.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-DEC-2000 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER FILE*(*)

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  At the moment, the config file only holds column definitions. Read
*  these.
      CALL POL1_COLNM( ' ', .FALSE., FILE, STATUS )

      END
