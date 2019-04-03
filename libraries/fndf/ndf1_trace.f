      SUBROUTINE NDF1_TRACE( ROUTIN, STATUS )
*+
*  Name:
*     NDF1_TRACE

*  Purpose:
*     Provide error traceback reporting for the NDF_ library.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_TRACE( ROUTIN, STATUS )

*  Description:
*     If error tracing is enabled, then when this routine is called
*     with a bad STATUS value it will report an error message
*     containing the name of the routine which called it. It is
*     intended to be used at the end of each routine in the NDF_
*     library.  A traceback of the routine calling sequence is then
*     obtained when a bad STATUS value is set in response to an error
*     condition, as a result of each routine exiting in sequence.

*  Arguments:
*     ROUTIN = CHARACTER * ( * ) (Given)
*        The name of the calling routine.
*     STATUS = INTEGER (Given and Returned)
*        The global status. The routine does not report an error if
*        this is set to SAI__OK or if error tracing is disabled.

*  Notes:
*     -  Error tracing can be enabled or disabled by means of calls to
*     NDF_TUNE (or NDF_TRACE, which is now obsolete).
*     -  This routine is intended to be called from Fortran. No
*     equivalent error tracing facility is currently available for
*     those NDF_ routines which are written in C.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-SEP-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     27-NOV-1989 (RFWS):
*        Added support for error tracing flag TCB_ETFLG.
*     28-NOV-1989 (RFWS):
*        Improved prologue.
*     5-NOV-1993 (RFWS):
*        Added TCB initialisation.
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
      INCLUDE 'NDF_TCB'          ! NDF_ Error Tracing Control Block
*        TCB_ETFLG = LOGICAL (Read)
*           Error tracing flag.

*  Arguments Given:
      CHARACTER * ( * ) ROUTIN

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      LOGICAL INIT               ! TCB initialised?

      SAVE INIT                  ! Remember if TCB initialised

*  Local Data:
      DATA INIT / .FALSE. /      ! TCB starts out un-initialised

*.

*  Check that the status value is bad, otherwise there is nothing more
*  to do.
      IF ( STATUS .NE. SAI__OK ) THEN

*  Ensure that the TCB is initialised. Do this inside a new error
*  reporting environment (otherwise the initialisation routine will not
*  execute).
         IF ( .NOT. INIT ) THEN
            CALL ERR_BEGIN( STATUS )
            CALL NDF1_INTCB( STATUS )

*  Note if initialisation succeeded.
            INIT = ( STATUS .EQ. SAI__OK )
            CALL ERR_END( STATUS )
         END IF

*  If TCB initialisation has been completed and the error tracing flag
*  is set, then an error report must be made.
         IF ( INIT .AND. TCB_ETFLG ) THEN

*  Define a message token for the routine name.
            CALL MSG_SETC( 'ROUTINE', ROUTIN )

*  Report an error traceback message.
            CALL ERR_REP( 'NDF1_TRACE_ERR',
     : '.....error exit from routine ^ROUTINE',
     :                    STATUS )
         END IF
      END IF

      END
