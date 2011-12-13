      SUBROUTINE NDF1_EXPPL( IPCB, PLACE, STATUS )
*+
*  Name:
*     NDF1_EXPPL

*  Purpose:
*     Export an NDF placeholder.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_EXPPL( IPCB, PLACE, STATUS )

*  Description:
*     The routine converts an index to an entry in the PCB into an NDF
*     placeholder which can be issued to an application to refer to
*     that entry. The placeholder issued is saved in the PCB so that a
*     check on its validity can later be made. NDF placeholders are
*     encoded so that it is extremely unlikely that two identical ones
*     will ever be issued, even if the NDF_ system is closed down
*     completely and restarted (a placeholder which is still valid can,
*     of course, never be duplicated). This makes it possible to detect
*     invalid placeholders and to report problems with "dangling"
*     placeholders if an application neglects to use them.

*  Arguments:
*     IPCB = INTEGER (Given)
*        Index to an entry in the PCB.
*     PLACE = INTEGER (Returned)
*        NDF placeholder for the PCB entry. A value of NDF__NOPL is
*        returned if an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     If this routine is called with STATUS not equal to SAI__OK, then
*     it returns a value of NDF__NOPL for the PLACE argument.

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
*     25-OCT-1993 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants

*  Global Variables:
      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        PCB_IDCTX = INTEGER (Read)
*           Current identifier context level.

      INCLUDE 'NDF_PCB'          ! NDF_ Placeholder Control Block
*        PCB_CHK( NDF__MXPCB ) = INTEGER (Write)
*           Record of placeholders issued, for subsequent checking.
*        PCB_CTX( NDF__MXDCB ) = INTEGER (Write)
*           Placeholder context level for each PCB entry.
*        PCB_PLCNT = INTEGER (Read and Write)
*           Count of placeholders issued so far.

*  Arguments Given:
      INTEGER IPCB

*  Arguments Returned:
      INTEGER PLACE

*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      EXTERNAL NDF1_INIT         ! Initialise common blocks

*  Local variables:
      INTEGER IDCODE             ! Value for encoding identifiers

*.

*  Set a default value of NDF__NOPL for the PLACE argument.
      PLACE = NDF__NOPL

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the PCB index is valid and report an error if it is not.
      IF ( ( IPCB .LT. 1 ) .OR. ( IPCB .GT. NDF__MXPCB ) ) THEN
         STATUS = NDF__FATIN
         CALL MSG_SETC( 'ROUTINE', 'NDF1_EXPPL' )
         CALL MSG_SETI( 'BADIPCB', IPCB )
         CALL ERR_REP( 'NDF1_EXPPL_IPCB',
     :   'Routine ^ROUTINE called with an invalid IPCB argument' //
     :   'of ^BADIPCB (internal programming error).', STATUS )

*  Increment the count of placeholders issued so far. This value is
*  never reset, so it allows unique placeholders to be generated (apart
*  from the effects of protecting against overflow - see below).
      ELSE
         PCB_PLCNT = PCB_PLCNT + 1

*  Encode the identifier, including protection against arithmetic
*  overflow if too many identifiers are issued.
         IDCODE = MOD( PCB_PLCNT, ( NUM__MAXI / NDF__MXPCB ) )
         PLACE = IPCB + NDF__MXPCB * IDCODE

*  Save the identifier in the PCB.
         PCB_CHK( IPCB ) = PLACE

*  Assign the current identifier context level to the PCB entry.
         PCB_CTX( IPCB ) = ACB_IDCTX
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_EXPPL', STATUS )

      END
