      SUBROUTINE NDF1_GETAP( APPN, STATUS )
*+
*  Name:
*     NDF1_GETAP

*  Purpose:
*     Get the name of the currently-executing application.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_GETAP( APPN, STATUS )

*  Description:
*     The routine returns the name of the currently-running
*     application, left justified. The returned value will be truncated
*     without error if the variable supplied is too short.

*  Arguments:
*     APPN = CHARACTER * ( * ) (Returned)
*        Application name.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This version is specific to the ADAM software environment. It
*     returns the current ADAM action name.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     13-MAY-1993 (RFWS):
*        Original version.
*     13-AUG-1993 (RFWS):
*        Implemented properly by calling TASK_GET_NAME.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Returned:
      CHARACTER * ( * ) APPN

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the current action name.
      CALL TASK_GET_NAME( APPN, STATUS )

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_GETAP', STATUS )

      END
