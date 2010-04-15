      SUBROUTINE IRAS90_PM( STATUS )
*+
*  Name:
*     IRAS90_PM

*  Purpose:
*     Top-level IRAS90 subroutine for A-task pseudo-monolith on Unix.

*  Language:
*     UNIX Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL IRAS90_PM( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This is the top-level pseudo-monolith subroutine for the IRAS90
*     suite of A-tasks.  It is a pseudo-monolith as the command does
*     not come from the ADAM interface, but is taken from the UNIX
*     command line.  Each IRAS90 command is an alias to a softlink that
*     points to this monolith.  The chosen commands is obtained from
*     the UNIX Fortran run-time library routine GETARG.  Given the
*     command,the requested A-task is called after a successful matching
*     of the input string with a valid task name.

*  Algorithm:
*     Check for error on entry - return if not o.k.
*     The input string NAME is tested against all the valid A-task
*     names after having been forced to upper-case. If a valid test
*     is made, the relevant A-task is called. If not, an error message
*     is output to the environment.

*  Implementation Deficiencies:
*     The input string has to be forced to upper-case.

*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     17-SEP-1992 (DSB):
*        Original version, modified from equivalent KAPPA routine
*        written by MJC.
*      8-AUG-2004 (TIMJ):
*        Use modern monolith style
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT  NONE              ! no implicit typing allowed

*  Global Constants:
      INCLUDE  'SAE_PAR'          ! SSE global definitions
      INCLUDE  'PAR_PAR'          ! Parameter system definitions

*  Status:
      INTEGER  STATUS

*  External References:

*  Local Variables:
      CHARACTER NAME*(PAR__SZNAM) ! Action name

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the action name.
      CALL TASK_GET_NAME( NAME, STATUS )

*  Identify and execute the task.
*  ==============================
*
*  Check the string against valid A-task names---if matched then call
*  the relevant A-task

      IF ( NAME .EQ. 'BACKCRDD' ) THEN
         CALL BACKCRDD( STATUS )

      ELSE IF ( NAME .EQ. 'COLCORR' ) THEN
         CALL COLCORR( STATUS )

      ELSE IF ( NAME .EQ. 'COLTEMP' ) THEN
         CALL COLTEMP( STATUS )

      ELSE IF ( NAME .EQ. 'COADDCRDD' ) THEN
         CALL COADDCRDD( STATUS )

      ELSE IF ( NAME .EQ. 'DEGLCRDD' ) THEN
         CALL DEGLCRDD( STATUS )

      ELSE IF ( NAME .EQ. 'DESTCRDD' ) THEN
         CALL DESTCRDD( STATUS )

      ELSE IF ( NAME .EQ. 'FINDCRDD' ) THEN
         CALL FINDCRDD( STATUS )

      ELSE IF ( NAME .EQ. 'HISTORY' ) THEN
         CALL HISTORY( STATUS )

      ELSE IF ( NAME .EQ. 'I90HELP' ) THEN
         CALL I90HELP( STATUS )

      ELSE IF ( NAME .EQ. 'IFIELD' ) THEN
         CALL IFIELD( STATUS )

      ELSE IF ( NAME .EQ. 'IRASTRACE' ) THEN
         CALL IRASTRACE( STATUS )

      ELSE IF ( NAME .EQ. 'MAKEPROF' ) THEN
         CALL MAKEPROF( STATUS )

      ELSE IF ( NAME .EQ. 'MAKERESP' ) THEN
         CALL MAKERESP( STATUS )

      ELSE IF ( NAME .EQ. 'MAPCRDD' ) THEN
         CALL MAPCRDD( STATUS )

      ELSE IF ( NAME .EQ. 'NEWUNITS' ) THEN
         CALL NEWUNITS( STATUS )

      ELSE IF ( NAME .EQ. 'POINTCRDD' ) THEN
         CALL POINTCRDD( STATUS )

      ELSE IF ( NAME .EQ. 'POSCRDD' ) THEN
         CALL POSCRDD( STATUS )

      ELSE IF ( NAME .EQ. 'PREPARE' ) THEN
         CALL PREPARE( STATUS )

      ELSE IF ( NAME .EQ. 'QUALTOBAD' ) THEN
         CALL QUALTOBAD( STATUS )

      ELSE IF ( NAME .EQ. 'REMQUAL' ) THEN
         CALL REMQUAL( STATUS )

      ELSE IF ( NAME .EQ. 'SETIMAGE' ) THEN
         CALL SETIMAGE( STATUS )

      ELSE IF ( NAME .EQ. 'SETQUAL' ) THEN
         CALL SETQUAL( STATUS )

      ELSE IF ( NAME .EQ. 'SIMCRDD' ) THEN
         CALL SIMCRDD( STATUS )

      ELSE IF ( NAME .EQ. 'SKYALIGN' ) THEN
         CALL SKYALIGN( STATUS )

      ELSE IF ( NAME .EQ. 'SKYBOX' ) THEN
         CALL SKYBOX( STATUS )

      ELSE IF ( NAME .EQ. 'SKYGRID' ) THEN
         CALL SKYGRID( STATUS )

      ELSE IF ( NAME .EQ. 'SKYLINE' ) THEN
         CALL SKYLINE( STATUS )

      ELSE IF ( NAME .EQ. 'SKYMARK' ) THEN
         CALL SKYMARK( STATUS )

      ELSE IF ( NAME .EQ. 'SKYPHOT' ) THEN
         CALL SKYPHOT( STATUS )

      ELSE IF ( NAME .EQ. 'SKYPOS' ) THEN
         CALL SKYPOS( STATUS )

      ELSE IF ( NAME .EQ. 'SKYWRITE' ) THEN
         CALL SKYWRITE( STATUS )

      ELSE IF ( NAME .EQ. 'SHOWQUAL' ) THEN
         CALL SHOWQUAL( STATUS )

      ELSE IF ( NAME .EQ. 'TRACECRDD' ) THEN
         CALL TRACECRDD( STATUS )

      ELSE

*  No such option exists.

         STATUS = SAI__ERROR
         CALL MSG_SETC( 'CMD', NAME )
         CALL ERR_REP( 'IRAS90_PM_NOCOM',
     :     'IRAS90: No such option ^CMD.', STATUS )

      END IF

*  End and return.

      END
