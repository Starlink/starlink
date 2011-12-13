      SUBROUTINE NDF1_ANNPL( ERASE, IPCB, STATUS )
*+
*  Name:
*     NDF1_ANNPL

*  Purpose:
*     Annul an NDF placeholder.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_ANNPL( ERASE, IPCB, STATUS )

*  Description:
*     The routine annuls an NDF placeholder, releasing the PCB slot
*     which it occupies and optionally erasing the object associated
*     with it.

*  Arguments:
*     ERASE = LOGICAL (Given)
*        Whether to erase the associated object.
*     IPCB = INTEGER (Given and Returned)
*        Index to placeholder entry in the PCB. A value of zero is
*        returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Regardless of the value of the ERASE argument, the associated
*     placeholder object will not be deleted unless its entry in the PCB
*     indicates that it was created by the NDF_ system (rather than
*     being a pre-existing object supplied by the caller).
*     -  This routine attempts to execute even if STATUS is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council

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
*     6-OCT-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     8-DEC-1989 (RFWS):
*        Installed NDF1_DELOB so that top-level placeholder objects can
*        be deleted if necessary.
*     1-MAR-1990 (RFWS):
*        Removed un-referenced include file.
*     9-MAR-1994 (RFWS):
*        Make use of the PCB_NEW flag.
*     11-MAR-1994 (RFWS):
*        Check for invalid PCB index values. Also revised to use
*        ERR_BEGIN and ERR_END.
*     25-APR-1994 (RFWS):
*        Ensure the PCB index is always reset to zero.
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
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Global Variables:
      INCLUDE 'NDF_PCB'          ! NDF_ Placeholder Control Block
*        PCB_LOC( NDF__MXPCB ) = CHARACTER * ( DAT__SZLOC ) (Read and
*        Write)
*           Placeholder locator.
*        PCB_NEW( NDF__MXPCB ) = LOGICAL (Read)
*           Whether placeholder object is a new one created by the NDF_
*           system.

*  Arguments Given:
      LOGICAL ERASE
      INTEGER IPCB

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Begin a new error reporting environment.
      CALL ERR_BEGIN( STATUS )

*  Check that the PCB index supplied is valid and report an error if it
*  is not.
      IF ( ( IPCB .LT. 1 ) .OR. ( IPCB .GT. NDF__MXPCB ) ) THEN
         STATUS = NDF__FATIN
         CALL MSG_SETC( 'ROUTINE', 'NDF1_ANNPL' )
         CALL MSG_SETI( 'IPCB', IPCB )
         CALL ERR_REP( 'NDF1_ANNPL_IPCB',
     :        'Routine ^ROUTINE called with an invalid IPCB ' //
     :        'argument of ^IPCB - internal programming error.',
     :        STATUS )

*  If required, and the placeholder object is a new one created by the
*  NDF_ system, then delete the associated object, annulling its locator
*  in the process.
      ELSE
         IF ( ERASE .AND. PCB_NEW( IPCB ) ) THEN
            CALL NDF1_DELOB( PCB_LOC( IPCB ), STATUS )

*  Otherwise, simply annul the locator.
         ELSE
            CALL DAT_ANNUL( PCB_LOC( IPCB ), STATUS )
         END IF

*  Release the PCB slot.
         CALL NDF1_RLS( NDF__PCB, IPCB, STATUS )
      END IF

*  Reset the PCB index.
      IPCB = 0

*  Call error tracing routine.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_ANNPL', STATUS )

*  End the error reporting environment.
      CALL ERR_END( STATUS )

      END
