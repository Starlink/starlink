      SUBROUTINE ARY_PLACE( LOC, NAME, PLACE, STATUS )
*+
*  Name:
*     ARY_PLACE

*  Purpose:
*     Obtain an array placeholder.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_PLACE( LOC, NAME, PLACE, STATUS )

*  Description:
*     The routine returns an array placeholder. A placeholder is used
*     to identify a position in the underlying data system (HDS) and
*     may be passed to other routines (e.g. ARY_NEW) to indicate where
*     a newly created array should be positioned.

*  Arguments:
*     LOC = CHARACTER * ( * ) (Given)
*        HDS locator to the structure to contain the new array.
*     NAME = CHARACTER * ( * ) (Given)
*        Name of the new structure component (i.e. the array).
*     PLACE = INTEGER (Returned)
*        Array placeholder identifying the nominated position in the
*        data system.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Placeholders are intended only for local use within an
*     application and only a limited number of them are available
*     simultaneously. They are always annulled as soon as they are
*     passed to another routine to create a new array, where they are
*     effectively exchanged for an array identifier.
*     -  If this routine is called with STATUS set, then a value of
*     ARY__NOPL will be returned for the PLACE argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason. The ARY__NOPL
*     constant is defined in the include file ARY_PAR.

*  Algorithm:
*     -  Set an initial value for the PLACE argument before checking the
*     inherited global status.
*     -  Check that a standard HDS component name has been supplied.
*     -  Obtain a free slot in the Placeholder Control Block.
*     -  Create a new placeholder object to reserve the position in the
*     data system.
*     -  Obtain a locator to the new object, storing it in the PCB and
*     linking it into a private group to prevent external events from
*     annulling it.
*     -  Increment the count of placeholders issued so far and encode
*     the PCB index into a new placeholder value.
*     -  Store the placeholder value for later validation.
*     -  If an error occurred, then release the PCB slot.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
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
*     19-SEP-1989 (RFWS):
*        Original version.
*     20-OCT-1989 (RFWS):
*        Added support for the PCB_TMP array controlling temporary
*        placeholders.
*     5-MAR-1990 (RFWS):
*        Added check that a standard HDS component name has been
*        supplied.
*     12-MAR-1990 (RFWS):
*        Changed placeholder type to ARRAY.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants

*  Global Variables:
      INCLUDE 'ARY_PCB'          ! ARY_ Placeholder Control Block
*        PCB_CHK( ARY__MXPCB ) = INTEGER (Write)
*           Placeholder check value for future validation.
*        PCB_LOC( ARY_MXPCB ) = CHARACTER * ( DAT__SZLOC ) (Write)
*           Locator to placeholder data object.
*        PCB_PLCNT = INTEGER (Read and Write)
*           Count of the number of placeholders issued so far.
*        PCB_TMP( ARY__MXPCB ) = LOGICAL (Write)
*           Whether the object which replaces the placeholder object
*           should be temporary.

*  Arguments Given:
      CHARACTER * ( * ) LOC
      CHARACTER * ( * ) NAME

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
      PLACE = ARY__NOPL

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that a standard HDS component name has been supplied.
      CALL ARY1_CHSCN( NAME, STATUS )

*  Obtain a free slot in the PCB.
      CALL ARY1_FFS( ARY__PCB, IPCB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Create a new array placeholder object to reserve a position in the
*  data system.
         DUMMY( 1 ) = 0
         CALL DAT_NEW( LOC, NAME, 'ARRAY', 0, DUMMY, STATUS )

*  Obtain a locator to the new object, storing it in the PCB and linking
*  it into a private group to prevent external events from annulling it.
         PCB_LOC( IPCB ) = ARY__NOLOC
         CALL DAT_FIND( LOC, NAME, PCB_LOC( IPCB ), STATUS )
         CALL HDS_LINK( PCB_LOC( IPCB ), 'ARY_PCB', STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Increment the count of placeholders issued so far.
            PCB_PLCNT = PCB_PLCNT + 1

*  Encode the PCB index into a placeholder value, making use of the
*  placeholder count and protecting against numerical overflow.
            PLCODE = MOD( PCB_PLCNT, ( NUM__MAXI / ARY__MXPCB ) )
            PLACE = IPCB + ARY__MXPCB * PLCODE

*  Store the placeholder value for later verification.
            PCB_CHK( IPCB ) = PLACE

*  Note that the object which replaces the placeholder object should not
*  be temporary.
            PCB_TMP( IPCB ) = .FALSE.

*  If an error occurred, then release the PCB slot.
         ELSE
            CALL ARY1_RLS( ARY__PCB, IPCB, STATUS )
         END IF
      END IF
       
*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARY_PLACE_ERR',
     :   'ARY_PLACE: Error obtaining array placeholder.', STATUS )
         CALL ARY1_TRACE( 'ARY_PLACE', STATUS )
      END IF

      END
