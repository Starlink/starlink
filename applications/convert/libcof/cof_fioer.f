      SUBROUTINE COF_FIOER( FSTAT, PARAM, ROUTIN, MESSGE, STATUS )
*+
*  Name:
*     COF_FIOER

*  Purpose:
*     Reports error messages associated with a call to a FITSIO routine.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_FIOER( FSTAT, PARAM, ROUTIN, MESSGE, STATUS )

*  Description:
*     This routine writes a standard error report for an error arising
*     within a FITSIO-library routine.  The report comprises two parts.
*     First there is a contextual error report.  The error number is
*     translated to an error string and appended to the supplied
*     message.  Thus the contextual message is of the form:
*        <ROUTIN>: <MESSGE>  Error was:
*     or
*        <MESSGE>  Error was:
*     when ROUTIN is a blank string.  After this header message, the
*     FITSIO error stack is flushed.  If for some strange reason, there
*     are no error messages in the stack, this routine appends the
*     short translation of the error number, and if there is no
*     translation, it reports the FITSIO error number and instruct the
*     reader to consult the FITSIO User's Guide.

*     The global status is set to SAI__ERROR.  This routine attempts
*     to work even if the global status is bad on entry.

*  Arguments:
*     FSTAT = INTEGER (Given)
*        The FITSIO status.
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter name of the error report.  It usually comprises
*        the name of the routine where the FITSIO routine is called
*        followed by some suitable suffix like '_ERR'.
*     ROUTIN = CHARACTER * ( * ) (Given)
*        The name of the FITSIO routine where the error occurred.
*        It may be blank to prevent it appearing in the error report.
*     MESSGE = CHARACTER * ( * ) (Given)
*        A contextual error message to form part of the error report.
*        It should indicate what was happening when the call to the
*        FITSIO routine was made.  No punctuation is added between
*        this and the "Error was" text, so a full stop should appear
*        in this message.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The error name for the FITSIO error messages is FITSIO_ERR.

*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1994 May 31 (MJC):
*        Original version.
*     1995 November 20 (MJC):
*        Changed to flush the new FITSIO error stack.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER FSTAT
      CHARACTER * ( * ) PARAM
      CHARACTER * ( * ) ROUTIN
      CHARACTER * ( * ) MESSGE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 80 ) FITBUF  ! Tranlated FITSIO error message
      LOGICAL LOOP               ! Loop to flush FITSIO error

*.

*  Do not check the inherited global status.

*  The FITSIO status should be positive.  Report the programming error
*  and exit.
      IF ( FSTAT .LE. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'COF_FIOER',
     :     'COF_FIOER: No FITSIO error to report (programming error).',
     :     STATUS )
         GOTO 999
      END IF

*  Report the FITSIO message.
*  ==========================

*  Set the global to be bad.
      STATUS = SAI__ERROR

*  Create a message token.
      CALL MSG_SETC( 'MESSG', MESSGE )

*  Make the introductory error report.  Use slightly different error
*  messages if no routine name is supplied or not.
      IF ( ROUTIN .EQ. ' ' ) THEN
         CALL ERR_REP( PARAM, '^MESSG   Error was:', STATUS )
      ELSE
         CALL MSG_SETC( 'ROUTINE', ROUTIN )
         CALL ERR_REP( PARAM, '^ROUTINE: ^MESSG   Error was: ',
     :                 STATUS )
      END IF

*  Flush the FITSIO error stack.
*  =============================

*  First check that there is some error to flush.
      CALL FTGMSG( FITBUF )
      LOOP = FITBUF .NE. ' '

      IF ( LOOP ) THEN
         CALL ERR_REP( 'FITSIO_ERR', FITBUF, STATUS )

*  Loop until the stack is flushed (indicated by FITBUF being a blank
*  string).
         DO WHILE ( LOOP )
            CALL FTGMSG( FITBUF )
            LOOP = FITBUF .NE. ' '
            IF ( LOOP ) CALL ERR_REP( 'FITSIO_ERR', FITBUF, STATUS )
         END DO

*  This is defensive programming, as the FITSIO documentation says that
*  there will always be an error stack if FITSIO has failed.  Report
      ELSE

*  Translate the FITSIO status into a short message.  If there is one
*  report it, otherwise just report the error number.
         CALL FTGERR( FSTAT, FITBUF )
         IF ( FITBUF .NE. ' ' ) THEN
            CALL ERR_REP( 'FITSIO_ERR', FITBUF, STATUS )

         ELSE
            CALL MSG_SETI( 'FSTAT', FSTAT )
            CALL ERR_REP( 'FITSIO_ERR', 'Error number ^FSTAT.  See '/
     :        /'the FITSIO User''s Guide (MUD/16) for details.',
     :        STATUS )
         END IF

      END IF
      
  999 CONTINUE
      END
