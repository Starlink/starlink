      SUBROUTINE CAT1_FIOER( FSTAT, PARAM, ROUTIN, MESSGE, STATUS )
*+
*  Name:
*     CAT1_FIOER

*  Purpose:
*     Reports error messages associated with a call to a FITSIO routine.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CAT1_FIOER( FSTAT, PARAM, ROUTIN, MESSGE, STATUS )

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

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 1995, 1998, 2000, 2004 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1994 May 31 (MJC):
*        Original version.
*     1995 November 20 (MJC):
*        Changed to flush the new FITSIO error stack.
*     22-JAN-1998 (DSB):
*        Supplied message is now expanded before being assigned to an MSG
*        token. This allows the supplied message to contain references to
*        other (pre-defined) message tokens.
*     1-FEB-2000 (DSB):
*        Enclose MSG_LOAD call in a new error reporting environment.
*     11-SEP-2000 (DSB):
*        Brought into POLPACK from CONVERT.
*     1-JUL-2004 (DSB):
*        Brought into CAT from POLPACK.
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
      CHARACTER * ( 80 ) OPSTR   ! Buffer for supplied message
      INTEGER OPLEN              ! Used length of OPSTR
      LOGICAL LOOP               ! Loop to flush FITSIO error

*.

*  Do not check the inherited global status.

*  Get the expanded message first so that substitutions occur for any
*  embedded message tokens.
      CALL MSG_LOAD( ' ', MESSGE, OPSTR, OPLEN, STATUS )

*  The FITSIO status should be positive.  Report the programming error
*  and exit.
      IF ( FSTAT .LE. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CAT1_FIOER',
     :     'CAT1_FIOER: No FITSIO error to report (programming error).',
     :     STATUS )
         GOTO 999
      END IF

*  Report the FITSIO message.
*  ==========================

*  Create a message token.
      CALL MSG_SETC( 'MESSG', OPSTR( : OPLEN ) )

*  Set the global to be bad.
      STATUS = SAI__ERROR

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
