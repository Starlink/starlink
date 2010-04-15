      SUBROUTINE IRCAMPACK_MON( STATUS )
*+
*  Name:
*     IRCAMPACK_MON

*  Purpose:
*     Top-level IRCAMPACK subroutine for A-task monolith on Unix.

*  Language:
*     UNIX Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL IRCAMPACK_MON( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This is the top-level A-task monolith subroutine for the IRCAMPACK
*     suite of A-tasks.  Each IRCAMPACK command is an alias to a softlink
*     that points to this monolith.  The chosen command is obtained
*     from the ADAM routine TASK_GET_NAME.  The command may be specified
*     from the shell or ICL.  Given the command, the requested A-task
*     is called after a successful matching of the input string with a
*     valid task name.  If there is no match, an error report is made.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-OCT-1993 (DSB):
*        Original version, modified from equivalent IRAS90 routine.
*     7-OCT-1996 (DSB):
*        Changed from old-style pseudo-monolith, to new ICL monolith.
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
      INTEGER CHR_LEN             ! Length of a character string less
                                  ! any trailing blanks

*  Local Variables:
      CHARACTER NAME*( PAR__SZNAM ) ! Action name

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the action name.
      CALL TASK_GET_NAME( NAME, STATUS )

*  Check the string against valid A-task names---if matched then call
*  the relevant A-task

      IF ( NAME .EQ. 'IRCAMSET' ) THEN
         CALL IRCAMSET( STATUS )

      ELSE IF ( NAME .EQ. 'IRCHELP' ) THEN
         CALL IRCHELP( STATUS )

      ELSE IF ( NAME .EQ. 'TNORM' ) THEN
         CALL TNORM( STATUS )

      ELSE IF ( NAME .EQ. 'CALPOL' ) THEN
         CALL CALPOL( STATUS )

      ELSE IF ( NAME .EQ. 'ERRCLIP' ) THEN
         CALL ERRCLIP( STATUS )

      ELSE IF ( NAME .EQ. 'VECPLOT' ) THEN
         CALL VECPLOT( STATUS )

      ELSE IF ( NAME .EQ. 'SEGMENT' ) THEN
         CALL SEGMENT( STATUS )

      ELSE IF ( NAME .EQ. 'CHECK_NDFNAME' ) THEN
	 CALL CHECK_NDFNAME( STATUS )

      ELSE

*  No such option exists.

         STATUS = SAI__ERROR
         CALL MSG_SETC( 'CMD', NAME )
         CALL ERR_REP( 'IRCAMPACK_PM_NOCOM',
     :     'IRCAMPACK: No such option ^CMD.', STATUS )

      END IF

*  End and return.

      END
