      SUBROUTINE NDF_HEND( STATUS )
*+
*  Name:
*     NDF_HEND

*  Purpose:
*     End NDF history recording for the current application.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_HEND( STATUS )

*  Description:
*     The routine closes down history recording for the current
*     application by writing default history information (when
*     appropriate) to any NDFs which are still active, and by flagging
*     that a new history record should be created to hold any
*     subsequent history information (ready for the next application).
*     If an application name has been declared via a call to NDF_HAPPN,
*     then that name is cleared.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine attempts to execute even if STATUS is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.
*     -  This routine should not normally be used by writers of NDF
*     applications. It is provided primarily to allow those writing
*     environment-level software to identify the end of an application
*     to the NDF_ system in circumstances where more than one
*     application may be invoked from within a single executing
*     program. Even then, it will not normally be needed unless NDF
*     data structures are intended to remain open throughout the
*     invocation of several applications and a separate history record
*     is required from each application.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     DSB: David S Berry (JC, UCLan)
*     {enter_new_authors_here}

*  History:
*     18-MAY-1993 (RFWS):
*        Original version.
*     19-MAY-1993 (RFWS):
*        Added clearing of current application name and changes to
*        ensure that a new history record is subsequently created with
*        default history information.
*     4-AUG-1993 (RFWS):
*        Improved the handling of error reporting environments.
*     28-SEP-1993 (RFWS):
*        Add logging of pending error messages.
*     23-JAN-2009 (DSB):
*        Added DCB_HTIME.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_HAPPN = CHARACTER * ( NDF__SZAPP ) (Write)
*           Name of the currently executing application.
*        DCB_HDEF( NDF__MXDCB ) = LOGICAL (Write)
*           Whether default history information is to be written.
*        DCB_HTLEN( NDF__MXDCB ) = INTEGER (Write)
*           Current history record text width.
*        DCB_HTIME( NDF__MXDCB ) = DOUBLE PRECISION (Write)
*           The date/time to attach to the next history record to be
*           created, as a UTC Modified Julian Date. If negative, then
*           the current time will be used.

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER NEXT               ! Next DCB slot to use

*.

*  Log any pending error message information for subsequent recording
*  in NDF history records.
      CALL NDF1_HLERR( STATUS )

*  Begin a new error reporting environment.
      CALL ERR_BEGIN( STATUS )

*  Loop through all the DCB entries still active within the current
*  application. Handle each within its own error reporting environment.
      IDCB = 0
 1    CONTINUE                   ! Start of 'DO WHILE' loop
      CALL ERR_BEGIN( STATUS )
      CALL NDF1_NXTSL( NDF__DCB, IDCB, NEXT, STATUS )
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( NEXT .NE. 0 ) ) THEN
         IDCB = NEXT

*  Write default history information (if necessary) to each data
*  object.
         CALL NDF1_HWDEF( IDCB, ' ', STATUS )

*  Dump any logged error message information to the history record.
         CALL NDF1_HDERR( IDCB, .FALSE., STATUS )

*  Reset the current history record text width to indicate that no
*  current history record exists (so that a new one will subsequently
*  be created). Also indicate that new default history information will
*  be required, and that he current time should be attached to the next
*  history record.
         DCB_HTLEN( IDCB ) = 0
         DCB_HDEF( IDCB ) = .TRUE.
         DCB_HTIME( IDCB ) = -1.0D0

*  End the current error reporting environment and return to process
*  the next DCB entry.
         CALL ERR_END( STATUS )
         GO TO 1
      END IF

*  End the last error reporting environment and clear the current
*  default application name used for history recording.
      CALL ERR_END( STATUS )
      DCB_HAPPN = ' '

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_HEND_ERR',
     :   'NDF_HEND: Error ending NDF history recording for the ' //
     :   'current application.', STATUS )
         CALL NDF1_TRACE( 'NDF_HEND', STATUS )
      END IF

*  End the outer error reporting environment.
      CALL ERR_END( STATUS )

      END
