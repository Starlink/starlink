      SUBROUTINE KAPGKS_MON( STATUS )
*+
*  Name:
*     KAPGKS_MON

*  Purpose:
*     Top-level KAPGKS subroutine for A-task monolith on UNIX.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL KAPGKS_MON( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This is the top-level A-task monolith subroutine for the KAPGKS
*     suite of A-tasks.  These are the remaining KAPPA applications with
*     GKS or IDI dependancies. As they are converted they should be moved 
*     from kapgks into kapview. Eventually, kapgks will be removed from
*     kappa once all applications have been converted.

*  Implementation Deficiencies:
*     The input string has to be forced to upper-case.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-NOV-2001 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT  NONE              ! no implicit typing allowed

*  Global Constants:
      INCLUDE  'SAE_PAR'          ! SSE global definitions

*  Status:
      INTEGER  STATUS

*  External References:
      LOGICAL LPG_AGAIN           ! Invoke the application again?

*  Local Variables:
      CHARACTER NAME * ( 15 )     ! Task name from the command
      LOGICAL DISAB               ! Disable looping?
      LOGICAL VERB                ! Run in verbose mode?
      REAL DELAY                  ! Delay between loops in seconds

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the command from the environment.  This returns uppercase
*  names.
      CALL TASK_GET_NAME( NAME, STATUS )

*  Identify and execute the task.
*  ==============================

*  Make CAT run in quiet mode.
      CALL CAT_TUNES( 'QUIET', 'YES', STATUS )

*  Define the current application name for history. The package version
*  number gets substituted in here when the KAPPA release source tar file 
*  is contructed.
      CALL NDF_HAPPN( NAME // ' (KAPPA 0.19)', STATUS )

*  See if NDF names should be reported when looping.
      CALL KPG1_ENVDF( 'KAPPA_REPORT_NAMES', VERB, STATUS )

*  If not, see if verbose reporting is required.
      IF( .NOT. VERB ) CALL KPG1_VERB( VERB, 'KAPPA', STATUS )      

*  See if looping should be disabled.
      CALL KPG1_ENVDF( 'KAPPA_LOOP_DISABLE', DISAB, STATUS )

*  See if a delay should be included between invocations.
      DELAY = 0.0
      CALL KPG1_ENV0R( 'KAPPA_LOOP_DELAY', DELAY, STATUS )

*  Initialise the common blocks used to control multiple invokation of
*  applications to process lists of NDFs.
      CALL LPG_START( VERB, DELAY, DISAB, STATUS )

*  Loop round invoking the task for each set of NDFs specified by the user.
      DO WHILE( LPG_AGAIN( STATUS ) )

*  Check the string against valid A-task names---if matched then call
*  the relevant A-task

*  Makes memory planes of an image-display device invisible.
         IF ( NAME .EQ. 'IDINVISIBLE' ) THEN
            CALL IDINVISIBLE ( STATUS )

*  Pans and zooms an image-display device.
         ELSE IF ( NAME .EQ. 'IDPAZO' ) THEN
            CALL IDPAZO ( STATUS )

*  Flips the colour table of an image-display device.
         ELSE IF ( NAME .EQ. 'LUTFLIP' ) THEN
            CALL LUTFLIP ( STATUS )

*  Highlights a colour table of image-display device.
         ELSE IF ( NAME .EQ. 'LUTHILITE' ) THEN
            CALL LUTHILITE ( STATUS )

*  Rotates a colour table of image-display device.
         ELSE IF ( NAME .EQ. 'LUTROT' ) THEN
            CALL LUTROT ( STATUS )

*  Tweaks a colour table of an image display.
         ELSE IF ( NAME .EQ. 'LUTTWEAK' ) THEN
            CALL LUTTWEAK ( STATUS )

         ELSE

*  No such option exists.
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'CMD', NAME )
            CALL ERR_REP( 'KAPGKS_MON_NOCOM',
     :     'KAPGKS: No such option ^CMD.', STATUS )

         END IF

      END DO

*  End and return.

      END

