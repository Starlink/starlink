      SUBROUTINE NDF1_HLERR( STATUS )
*+
*  Name:
*     NDF1_HLERR

*  Purpose:
*     Log error message information for NDF history recording.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_HLERR( STATUS )

*  Description:
*     If called with STATUS set, the routine extracts any pending EMS_
*     error messages and stores them in the Error Logging Block (ELB)
*     for subsequent recording in NDF history records. The EMS_ error
*     table is restored to its previous state by re-reporting the error
*     messages so that none are lost. If STATUS is not set on entry, the
*     ELB is simply cleared of error messages.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine calls EMS_ routines (rather then ERR_ routines).
*     This is in order to avoid interference from environment-specific
*     interpretation of error message text (e.g. the ADAM environment
*     may replace reported error messages with an externally supplied
*     version), which might make it impossible to restore the error
*     stack to its original state.

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     3-JUN-1993 (RFWS):
*        Original version.
*     12-MAY-1994 (RFWS):
*        Adapted for new behaviour of EMS_ELOAD.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'EMS_PAR'          ! EMS_ public constants

*  Global Variables:
      INCLUDE 'NDF_ELB'          ! NDF_ Error Logging Block
*        ELB_MSG( NDF__MXERR ) = CHARACTER * ( EMS__SZMSG ) (Write)
*           Pending error message text.
*        ELB_NERR = INTEGER (Write)
*           Number of pending error messages.
*        ELB_STAT = INTEGER (Write)
*           Status value associated with last message.

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( EMS__SZMSG ) MSG ! Error message text
      CHARACTER * ( EMS__SZPAR ) NAME ! Error name
      INTEGER I                  ! Loop counter for error messages
      INTEGER ISTAT              ! Temporary status value
      INTEGER LMSG               ! Length of error message
      INTEGER LNAME              ! Length of message name
      INTEGER STAT               ! Pending error message status value

*.

*  Initialise the number of pending error messages and the logged
*  status value. Then simply return if STATUS is not set.
      ELB_NERR = 0
      ELB_STAT = STATUS
      IF ( STATUS .EQ. SAI__OK ) RETURN

*  If STATUS is set, then inquire whether there are any error messages
*  pending.
      CALL EMS_STAT( ISTAT )

*  If not, then simply leave the error count at zero. Otherwise, loop to
*  load pending error messages into the ELB until no error status is
*  returned (indicating that all the messages have been read).
      IF ( ISTAT .NE. SAI__OK ) THEN
 1       CONTINUE                ! Start of 'DO WHILE' loop
         CALL EMS_ELOAD( NAME, LNAME, MSG, LMSG, ISTAT )
         IF ( ISTAT .NE. SAI__OK ) THEN
            LMSG = MAX( 1, LMSG )

*  Re-report each error (using the associated name and status value) so
*  that the error stack eventually ends up with the same contents as it
*  started with.
            CALL EMS_SETC( 'MSG', MSG( : LMSG ) )
            CALL EMS_REP( NAME, '^MSG', ISTAT )

*  If there is still room, enter each message into the ELB.
            IF ( ELB_NERR .LT. NDF__MXERR ) THEN
               ELB_NERR = ELB_NERR + 1
               ELB_MSG( ELB_NERR ) = MSG( : LMSG )
            END IF

*  Return to process the next error message.
            GO TO 1
         END IF
      END IF

      END
