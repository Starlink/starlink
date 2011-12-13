      SUBROUTINE COI_FIOER( ISTAT, PARAM, ROUTIN, MESSGE, STATUS )
*+
*  Name:
*     COI_FIOER

*  Purpose:
*     Reports error messages associated with a call to a IMFORT routine.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COI_FIOER( ISTAT, PARAM, ROUTIN, MESSGE, STATUS )

*  Description:
*     This routine writes a standard error report for an error arising
*     within a IRAF IMFORT-library routine.  The report comprises two
*     parts.  First there is a contextual error report.  The error
*     number is translated to an error string and appended to the
*     supplied message.  Thus the contextual message is of the form:
*        <ROUTIN>: <MESSGE>  Error was:
*     or
*        <MESSGE>  Error was:
*     when ROUTIN is a blank string.  After this header message, the
*     IMFORT error message is flushed.
*
*     The global status is set to SAI__ERROR.  This routine attempts
*     to work even if the global status is bad on entry.

*  Arguments:
*     ISTAT = INTEGER (Given)
*        The FITSIO status.
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter name of the error report.  It usually comprises
*        the name of the routine where the IMFORT routine is called
*        followed by some suitable suffix like '_ERR'.
*     ROUTIN = CHARACTER * ( * ) (Given)
*        The name of the IMFORT routine where the error occurred.
*        It may be blank to prevent it appearing in the error report.
*     MESSGE = CHARACTER * ( * ) (Given)
*        A contextual error message to form part of the error report.
*        It should indicate what was happening when the call to the
*        IMFORT routine was made.  No punctuation is added between
*        this and the "Error was" text, so a full stop should appear
*        in this message.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The error name for the IMFORT error messages is IMFORT_ERR.

*  Copyright:
*     Copyright (C) 1997 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1997 March 25 (MJC):
*        Original version.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER ISTAT
      CHARACTER * ( * ) PARAM
      CHARACTER * ( * ) ROUTIN
      CHARACTER * ( * ) MESSGE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER IMOK               ! Good status for IMFORT
      PARAMETER( IMOK = 0 )

*  Local Variables:
      CHARACTER * ( 80 ) IMFBUF  ! Tranlated IMFORT error message

*.

*  Do not check the inherited global status.

*  The FITSIO status should not be OK
*  and exit.
      IF ( ISTAT .EQ. IMOK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'COI_FIOER',
     :     'COI_FIOER: No IMFORT error to report (programming error).',
     :     STATUS )
         GOTO 999
      END IF

*  Report the IMFORT message.
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

*  Flush the IMFORT error.
*  =======================

*  First check that there is some error to flush.
      CALL IMEMSG( ISTAT, IMFBUF )
      CALL ERR_REP( 'IMFORT_ERR', IMFBUF, STATUS )

  999 CONTINUE

      END
