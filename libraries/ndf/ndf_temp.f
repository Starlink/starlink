      SUBROUTINE NDF_TEMP( PLACE, STATUS )
*+
*  Name:
*     NDF_TEMP

*  Purpose:
*     Obtain a placeholder for a temporary NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_TEMP( PLACE, STATUS )

*  Description:
*     The routine returns an NDF placeholder which may be used to
*     create a temporary NDF (i.e. one which will be deleted
*     automatically once the last identifier associated with it is
*     annulled). The placeholder returned by this routine may be passed
*     to other routines (e.g. NDF_NEW or NDF_COPY) to produce a
*     temporary NDF in the same way as a new permanent NDF would be
*     created.

*  Arguments:
*     PLACE = INTEGER (Returned)
*        Placeholder for a temporary NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Placeholders are intended only for local use within an
*     application and only a limited number of them are available
*     simultaneously. They are always annulled as soon as they are
*     passed to another routine to create a new NDF, where they are
*     effectively exchanged for an NDF identifier.
*     -  If this routine is called with STATUS set, then a value of
*     NDF__NOPL will be returned for the PLACE argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason. The NDF__NOPL
*     constant is defined in the include file NDF_PAR.

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
*     18-OCT-1989 (RFWS):
*        Original, derived from the NDF_PLACE routine.
*     26-FEB-1990 (RFWS):
*        Changed the NDF placeholder data type to 'NDF'.
*     11-NOV-1993 (RFWS):
*        Changed to call NDF1_EXPPL to export the placeholder value.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants

*  Global Variables:
      INCLUDE 'NDF_PCB'          ! NDF_ Placeholder Control Block
*        PCB_LOC( NDF__MXPCB ) = CHARACTER * ( DAT__SZLOC ) (Write)
*           Locator to placeholder data object.
*        PCB_TMP( NDF__MXPCB ) = LOGICAL (Write)
*           Whether the object which replaces the placeholder should be
*           temporary.

*  Arguments Returned:
      INTEGER PLACE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER DUMMY( 1 )         ! Dummy dimension array
      INTEGER IPCB               ! Index to placeholder entry in the PCB
      INTEGER PLCODE             ! Value for encoding placeholders

*.

*  Set an initial value for the PLACE argument.
      PLACE = NDF__NOPL

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain a free slot in the PCB.
      CALL NDF1_FFS( NDF__PCB, IPCB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Create a temporary NDF placeholder object, storing a locator to it
*  in the PCB.
         DUMMY( 1 ) = 0
         CALL NDF1_TEMP( 'NDF', 0, DUMMY, PCB_LOC( IPCB ), STATUS )

*  Link the locator into a private group to prevent external events
*  from annulling it.
         CALL HDS_LINK( PCB_LOC( IPCB ), 'NDF_PCB', STATUS )

*  Note the object to replace the placeholder is to be temporary.
         PCB_TMP( IPCB ) = .TRUE.

*  Export the required placeholder
         CALL NDF1_EXPPL( IPCB, PLACE, STATUS )

*  If an error occurred, then annul the PCB entry.
         IF ( STATUS .NE. SAI__OK ) CALL NDF1_ANNPL( .TRUE., IPCB,
     :                                               STATUS )
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_TEMP_ERR',
     :   'NDF_TEMP: Error obtaining a placeholder for a temporary NDF.',
     :   STATUS )
         CALL NDF1_TRACE( 'NDF_TEMP', STATUS )
      END IF

      END
