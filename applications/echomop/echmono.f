      SUBROUTINE ECHMONO( STATUS )
*+
*  Name:
*     ECHMONO

*  Purpose:
*     Top-level ADAM monolith routine for the ECHOMOP package.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ECHMONO( STATUS )

*  Description:
*     This routine administrates start-up and shut-down of ECHOMOP.
*     Interpretation of the selected action is handled by the ECHOMOP
*     routine.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     04-APR-1990 (DMILLS):
*       Initial release.
*     18-JUL-1996 (MJC):
*       Prologue, new startup banner.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_CONTEXT.INC'
      INCLUDE 'ECH_MODULES.INC'
      INCLUDE 'ECH_MAPPING.INC'
      INCLUDE 'ECH_ECHOMOP.INC'
      INCLUDE 'ECH_SERVER.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'

*  Arguments Given and Returned:

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I
      INTEGER IFROM

      LOGICAL STARTED
      LOGICAL VERSIONONLY

      CHARACTER*16 ACTION

*  Data Statements:
      DATA STARTED / .FALSE. /
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set context variable denoting control mechanism to denote monolith.
      CALL ECH_SET_CONTEXT( 'CONTROL', 'MONOLITH' )

*  Find out if we want a menu or just a task invocation.
      CALL TASK_GET_NAME( ACTION, STATUS )
      IFROM = 1
      I = 1
      DO WHILE ( ACTION( I : I ) .NE. ' ' )
         IF ( ACTION( I : I ) .EQ. '/' ) IFROM = I + 1
         I = I + 1
      END DO
      ACTION = ACTION( IFROM : I - 1 )

*  If menu task, issue welcome, and display the version number.
      IF ( ACTION .EQ. 'ECHMENU' ) THEN
*      %%VERSION%%
         CALL ECH_REPORT( 0, 'This is ECHOMOP Version 3.3-7' )
      END IF

*   If the VERSION parameter was present and true, then exit,
*   .having displayed the version number
      CALL PAR_GET0L( 'VERSION', VERSIONONLY, STATUS )
      IF ( VERSIONONLY ) GO TO 999

*  Initialise common areas.
      IF ( .NOT. STARTED ) THEN
         CALL ECH_INITIALISE( STATUS )
         STARTED = .TRUE.
      END IF

*  Return immediately if initialisation failed.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Pass control to ECHOMOP.
      CALL ECHOMOP( ACTION, STATUS )

*  Flush any errors.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
      END IF

      CALL ECH_MODULE_TIDYUP( 'ECHOMOP', STATUS )
      CALL ECH_CLOSEDOWN( STATUS )

  999 IF ( STATUS .EQ. ECH__ABORT_OPTION ) THEN
         STATUS = SAI__OK

      ELSE IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

      END
