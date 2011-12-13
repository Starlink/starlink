      SUBROUTINE NDF1_NPLAC( LOC, NAME, IPCB, STATUS )
*+
*  Name:
*     NDF1_NPLAC

*  Purpose:
*     Create an NDF placeholder entry in the PCB.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_NPLAC( LOC, NAME, IPCB, STATUS )

*  Description:
*     The routine creates an NDF placeholder entry in the PCB for a new
*     NDF. This is used to identify a position in the underlying data
*     system and may be passed to other routines to indicate where a
*     newly created NDF should be positioned.

*  Arguments:
*     LOC = CHARACTER * ( * ) (Given)
*        HDS locator which, inconjunction with the NAME argument,
*        identifies the structure which is to become a new NDF. A value
*        of DAT__ROOT may be supplied to indicate that the NAME
*        argument contains an absolute object name.
*     NAME = CHARACTER * ( * ) (Given)
*        Name to be used together with the LOC value to identify the
*        placeholder object. If LOC is set to DAT__ROOT, this should be
*        the absolute HDS name of the object, otherwise it should be a
*        relative name.
*     IPCB = INTEGER (Returned)
*        Index to a new PCB entry identifying the nominated position in
*        the data system.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If the object identified by LOC and NAME exists before this
*     routine is invoked (and it is not a top-level object), then it
*     should be an empty scalar structure of type 'NDF', otherwise an
*     error will result.
*     -  If this routine is called with STATUS set, then a value of
*     zero will be returned for the IPCB argument, although no further
*     processing will occur. The same value will also be returned if
*     the routine should fail for any reason.

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
*     4-NOV-1993 (RFWS):
*        Original version.
*     9-MAR-1994 (RFWS):
*        Added PCB_NEW as argument to NDF1_PLCRE.
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

*  Global Variables:
      INCLUDE 'NDF_PCB'          ! NDF_ Placeholder Control Block
*        PCB_LOC( NDF__MXPCB ) = CHARACTER * ( DAT__SZLOC ) (Write)
*           Locator to placeholder data object.
*        PCB_NEW( NDF__MXPCB ) = LOGICAL (Write)
*           Whether a new placeholder object was created.

*  Arguments Given:
      CHARACTER * ( * ) LOC
      CHARACTER * ( * ) NAME

*  Arguments Returned:
      INTEGER IPCB

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Set an initial null value for the IPCB argument.
      IPCB = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain a free slot in the PCB.
      CALL NDF1_FFS( NDF__PCB, IPCB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Create the placeholder object, storing its locator in the PCB.
         CALL NDF1_PLCRE( LOC, NAME, PCB_LOC( IPCB ), PCB_NEW( IPCB ),
     :                    STATUS )

*  If an error occurred, then release the PCB slot.
         IF ( STATUS .NE. SAI__OK ) CALL NDF1_RLS( NDF__PCB, IPCB,
     :                                             STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_NPLAC', STATUS )

      END
