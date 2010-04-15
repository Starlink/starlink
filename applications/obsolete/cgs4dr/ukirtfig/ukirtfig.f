      SUBROUTINE UKIRTFIG( STATUS )
*+
*  Name:
*     UKIRTFIG
*  Purpose:
*     Top-level ADAM monolith routine for the UKIRT FIGARO package.
*  Language:
*     Starlink Fortran 77
*  Invocation:
*     CALL UKIRTFIG( STATUS )
*  Description:
*     This routine interprets the action name passed to it and calls
*     the appropriate routine to perform the specified action. An error
*     will be reported and STATUS will be set if the action name is not
*     recognised.
*  Arguments:
*     ACTION = CHARACTER * ( 32 ) (Given and Returned)
*        The action name to be interpreted. The value given will be
*        forced to upper case by this routine.
*        On Unix platforms, ACTION is not got through the subroutine
*        argument list, but by a call to GETARG( 0, ).
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Notes:
*     This source file is intended for the Unix version only.
*  Authors:
*     PND: Phil Daly (JAC, Hawaii)
*     {enter_new_authors_here}
*  History:
*     14-AUG-1995 (PND): Re-written to conform to Unix standard
*     {enter_further_changes_here}
*  Bugs:
*     {note_any_bugs_here}
*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_PAR'          ! Standard PAR constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL BATCH
      INTEGER IGNORE
      CHARACTER * ( PAR__SZNAM ) ACTION
      CHARACTER * ( 8 ) ENVVAR

*  Internal References:
      INTEGER ICH_FOLD

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the action name.
      CALL TASK_GET_NAME( ACTION, STATUS )
      IGNORE = ICH_FOLD( ACTION )

*  Find out about the batch mode.
      CALL PSX_GETENV( 'FIGARO_MODE', ENVVAR, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         BATCH = .FALSE.
      ELSE
         IGNORE = ICH_FOLD( ENVVAR )
         IF ( ENVVAR .EQ. 'BATCH' ) THEN
            BATCH = .TRUE.
         ELSE
            BATCH = .FALSE.
         END IF
      END IF

*  Initialise the (F)PAR common block.
      CALL PAR_INIT( ACTION, ' ', 0, BATCH, IGNORE )

*  Test the action name against each valid value in turn, calling the
*  appropriate routine...
      IF ( ACTION .EQ. 'CGS3LIST' ) THEN
         CALL CGS3LIST
      ELSE IF ( ACTION .EQ. 'CGS4LIST' ) THEN
        CALL CGS4LIST
      ELSE IF ( ACTION .EQ. 'IRCAM3LIST' ) THEN
        CALL IRCAM3LIST
      ELSE IF ( ACTION .EQ. 'WASCII' ) THEN
        CALL WASCII
*     ELSE IF ( ACTION .EQ. 'UKARC' ) THEN
*       CALL UKARC
      ELSE IF ( ACTION .EQ. 'STATUS' ) THEN
        CALL PAR_WRUSER( ' ', STATUS )
        CALL PAR_WRUSER( 'UKIRTFIG Task: Portable-CGS4DR UKIRTFIG VPKG_VERS', STATUS )
        CALL PAR_WRUSER( 'UKIRTFIG Task: The uncached UKIRTFIG task is OK', STATUS )
        CALL PAR_WRUSER( ' ', STATUS )

*     [ADAM_action]...

*  If the action name is not recognised,
*  give some hints for experienced and disappointed users.
      ELSE
         CALL FIG_HELP( 'diversion', STATUS )
      END IF

      END
