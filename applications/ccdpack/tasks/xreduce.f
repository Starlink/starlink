      SUBROUTINE XREDUCE( PID, STATUS )
*+
*  Name:
*     XREDUCE

*  Purpose:
*     Starts up the XREDUCE command.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL XREDUCE( PID, STATUS )

*  Arguments:
*     PID = CHARACTER * ( * )
*        The PID string for any process created by the SLV routines
*        (not used in this routine).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine runs up the XREDUCE application. It is necessary
*     as we need to run all commands as task under IRAF/CL. This
*     ensures that we share the same environment (variables) as CL.

*  Usage:
*     RUNXREDUCE

*  ADAM Parameters:
*     NONE

*  Notes:
*     This routine is undocumented. It should work as described in the
*     XREDUCE documentation, except that the process is run in the
*     background, so that it doesn't block other CL interactions.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     5-JUN-1997 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants

*  Status:
      CHARACTER * ( * ) PID     ! Task identifier string
      INTEGER STATUS            ! Global status

*  External References:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN          ! Used length of string

*  Local Variables:
      CHARACTER * ( 132 ) CCDDIR ! CCDPACK_DIR

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      CALL PSX_GETENV( 'CCDPACK_DIR', CCDDIR, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_OUT( ' ', 'Starting the XREDUCE interface', STATUS )
         CCDDIR = CCDDIR( :CHR_LEN( CCDDIR ) ) // '/xreduce &'
         CALL CCD1_EXEC( CCDDIR, STATUS )
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( 'XREDUCE_ERR',
     :               'XREDUCE: failed to start XREDUCE.', STATUS )
      END IF

      END
