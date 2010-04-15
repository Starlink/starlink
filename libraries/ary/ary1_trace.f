      SUBROUTINE ARY1_TRACE( ROUTIN, STATUS )
*+
*  Name:
*     ARY1_TRACE

*  Purpose:
*     Provide error traceback reporting for the ARY_ library.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_TRACE( ROUTIN, STATUS )

*  Description:
*     If error tracing is enabled, then when this routine is called
*     with a bad STATUS value, it will report an error message
*     containing the name of the routine which called it. It is
*     intended to be used at the end of each routine in the ARY_
*     library.  A traceback of the routine calling sequence is then
*     obtained when a bad STATUS value is set in response to an error
*     condition, as a result of each routine exiting in sequence.

*  Arguments:
*     ROUTIN = CHARACTER * ( * ) (Given)
*        The name of the calling routine.
*     STATUS = INTEGER (Given)
*        The global status. The routine does not report an error if
*        this is set to SAI__OK.

*  Notes:
*     -  Error tracing can be enabled or disabled by means of calls to
*     ARY_TRACE.

*  Algorithm:
*     -  Check that the STATUS value is bad and error tracing is
*     enabled.
*     -  Define a message token for the calling routine's name.
*     -  Report an error traceback message.

*  Copyright:
*     Copyright (C) 1989 Science & Engineering Research Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-MAY-1989 (RFWS):
*        Original version.
*     22-NOV-1989 (RFWS):
*        Added support for the error tracing flag in the TCB.
*     30-NOV-1989 (RFWS):
*        Improved prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants

*  Global Variables:
      INCLUDE 'ARY_TCB'          ! ARY_ Error Tracing Control Block
*        TCB_ETFLG = LOGICAL (Read)
*           Error tracing flag.

*  Arguments Given:
      CHARACTER * ( * ) ROUTIN

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check the STATUS value is bad and error tracing is enabled.
      IF ( ( STATUS .NE. SAI__OK ) .AND. TCB_ETFLG ) THEN

*  Define a message token for the routine name.
         CALL MSG_SETC( 'ROUTINE', ROUTIN )

*  Report an error traceback message.
         CALL ERR_REP( ' ', '.....error exit from routine ^ROUTINE',
     :    STATUS )
      END IF

      END
