      SUBROUTINE NDF1_PLDCB( IPCB, IDCB, STATUS )
*+
*  Name:
*     NDF1_PLDCB

*  Purpose:
*     Initialise a DCB entry from a placeholder.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_PLDCB( IPCB, IDCB, STATUS )

*  Description:
*     The routine copies information from a placeholder entry in the
*     PCB into a newly-created DCB entry in order to initialise those
*     properties which a new data object inherits from its parent
*     placeholder. These include the data object itself, its attributes,
*     and information about associated foreign data files and the
*     object's disposal mode (e.g. when creating temporary objects).

*  Arguments:
*     IPCB = INTEGER (Given)
*        Index to the placeholder entry in the DCB.
*     IDCB = INTEGER (Given)
*        Index to the DCB entry to be initialised.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The PCB entry is not modified by this routine.

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
*     11-NOV-1993 (RFWS):
*        Also copy the keep NDF objects flag.
*     11-NOV-1993 (RFWS):
*        Clear the DCB foreign file existence flag.
*     29-APR-1994 (RFWS):
*        Changed to initialise the data object locator, the file and
*        path names, and to mark the container file as scratch if
*        necessary.
*     25-MAY-1994 (RFWS):
*        Initialise DCB_FORID from the corresponding PCB value.
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
*        DCB_DSP( NDF__MXDCB ) = CHARACTER * ( NDF__SZDSP ) (Write)
*           Data object disposal mode.
*        DCB_FILE( NDF__MXDCB ) = CHARACTER * ( NDF__SZFIL ) (Write)
*           Data object container file name.
*        DCB_FORFL( NDF__MXDCB ) = CHARACTER * ( NDF__SZFIL ) (Write)
*           Associated foreign file name.
*        DCB_FORID( NDF__MXDCB ) = CHARACTER * ( NDF__SZFID ) (Write)
*           Foreign file identification code.
*        DCB_FORKP( NDF__MXDCB ) = LOGICAL (Write)
*           Whether the NDF copy of the foreign file is to be kept.
*        DCB_IFMT( NDF__MXDCB ) = INTEGER (Write)
*           Foreign data format code.
*        DCB_LOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Write)
*           Data object locator.
*        DCB_MOD( NDF__MXDCB ) = CHARACTER * ( NDF__SZMOD ) (Write)
*           The NDF's access mode.
*        DCB_PATH( NDF__MXDCB ) = CHARACTER * ( NDF__SZPTH ) (Write)
*           Data object HDS path name.

      INCLUDE 'NDF_PCB'          ! NDF_ Placeholder Control Block
*        PCB_FORFL( NDF__MXPCB ) = CHARACTER * ( NDF__SZFIL ) (Read)
*           Associated foreign file name.
*        PCB_FORID( NDF__MXPCB ) = CHARACTER * ( NDF__SZFID ) (Read)
*           Foreign file identification code.
*        PCB_FORKP( NDF__MXPCB ) = LOGICAL (Read)
*           Whether the NDF copy of the foreign file is to be kept.
*        PCB_IFMT( NDF__MXPCB ) = INTEGER (Read)
*           Foreign data format code.
*        PCB_LOC( NDF__MXPCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator to placeholder object.
*        PCB_TMP( NDF__MXPCB ) = LOGICAL (Read)
*           Whether object should be temporary.

*  Arguments Given:
      INTEGER IPCB
      INTEGER IDCB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER NLEV               ! HDS object nesting level

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Clone a locator to the PCB data object for storage in the DCB.
*  Promote it to be a primary locator and link it into a private group
*  to prevent external events annulling it.
      CALL DAT_CLONE( PCB_LOC( IPCB ), DCB_LOC( IDCB ), STATUS )
      CALL DAT_PRMRY( .TRUE., DCB_LOC( IDCB ), .TRUE., STATUS )
      CALL HDS_LINK( DCB_LOC( IDCB ), 'NDF_DCB', STATUS )

*  Obtain the data object file and path names and enter them into the
*  DCB.
      CALL HDS_TRACE( DCB_LOC( IDCB ), NLEV, DCB_PATH( IDCB ),
     :                DCB_FILE( IDCB ), STATUS )

*  Copy the PCB foreign file format code to the DCB.
      DCB_IFMT( IDCB ) = PCB_IFMT( IPCB )

*  If this code is non-zero, also copy the foreign file name, its
*  identification code and the flag indicating whether the NDF copy of
*  the foreign file is to be kept.
      IF ( DCB_IFMT( IDCB ) .NE. 0 ) THEN
         DCB_FORFL( IDCB ) = PCB_FORFL( IPCB )
         DCB_FORID( IDCB ) = PCB_FORID( IPCB )
         DCB_FORKP( IDCB ) = PCB_FORKP( IPCB )

*  If the data object (when converted from a foreign format file) is not
*  to be kept, then mark its container file as a scratch file to ensure
*  it will be deleted when no longer required.
         IF ( .NOT. DCB_FORKP( IDCB ) ) THEN
            CALL NDF1_HSCRT( DCB_LOC( IDCB ), STATUS )
         END IF
      END IF

*  Set the DCB disposal mode if the PCB indicates the object is to be
*  temporary.
      IF ( PCB_TMP( IPCB ) ) DCB_DSP( IDCB ) = 'TEMP'

*  Set the data object's DCB access mode.
      DCB_MOD( IDCB ) = 'UPDATE'

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_PLDCB', STATUS )

      END
