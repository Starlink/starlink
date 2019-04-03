      SUBROUTINE NDF_PLACE( LOC, NAME, PLACE, STATUS )
*+
*  Name:
*     NDF_PLACE

*  Purpose:
*     Obtain an NDF placeholder.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_PLACE( LOC, NAME, PLACE, STATUS )

*  Description:
*     The routine returns an NDF placeholder. A placeholder is used
*     to identify a position in the underlying data system (HDS) and
*     may be passed to other routines (e.g. NDF_NEW) to indicate where
*     a newly created NDF should be positioned.

*  Arguments:
*     LOC = CHARACTER * ( * ) (Given)
*        HDS locator to the structure to contain the new NDF.
*     NAME = CHARACTER * ( * ) (Given)
*        Name of the new structure component (i.e. the NDF).
*     PLACE = INTEGER (Returned)
*        NDF placeholder identifying the nominated position in the
*        data system.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Placeholders are intended only for local use within an
*     application and only a limited number of them are available
*     simultaneously. They are always annulled as soon as they are
*     passed to another routine to create a new NDF, where they are
*     effectively exchanged for an NDF identifier.
*     -  The value given for the NAME argument may be an HDS path name,
*     consisting of several fields separated by '.', so that an NDF can
*     be created in a sub-component (or a sub-sub-component...) of the
*     structure identified by the locator LOC.  Array subscripts may
*     also be used in this component name.  Thus a string such as
*     'MYSTRUC.ZONE(2).IMAGE' could be used as a valid NAME value.
*     -  Normally, this routine will be used as the basis for creating
*     a completely new NDF data structure. However, if the LOC and NAME
*     arguments refer to a pre-existing object, then this structure
*     will be used as the basis for the new NDF.  An object which is to
*     be used in this way must be an empty scalar structure with an HDS
*     type of 'NDF'.
*     -  A new NDF can be created within an explicitly named container
*     file by supplying the symbolic value DAT__ROOT for the LOC
*     argument, and specifying the container file within the value
*     supplied for the NAME argument. If the object is the top level
*     object within a container file, then a new container file is
*     created. If it is not a top level object, then the container file
*     and all structures lying above the object should already exist.
*     -  If a blank value is given for the NAME argument, then the new
*     NDF will be the object identified directly by the locator LOC.
*     This must be an empty scalar structure of type 'NDF'.
*     -  If this routine is called with STATUS set, then a value of
*     NDF__NOPL will be returned for the PLACE argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason.
*     -  The NDF__NOPL constant is defined in the include file NDF_PAR.
*     The DAT__ROOT constant is defined in the include file DAT_PAR
*     (see SUN/92).

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
*     11-AUG-1993 (RFWS):
*        New public routine which calls the internal routine to which
*        all the code has now been moved.
*     3-NOV-1993 (RFWS):
*        Changed to call NDF1_EXPPL to export the placeholder value.
*     4-NOV-1993 (RFWS):
*        Changed to support foreign format files.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants

*  Arguments Given:
      CHARACTER * ( * ) LOC
      CHARACTER * ( * ) NAME

*  Arguments Returned:
      INTEGER PLACE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IPCB               ! Index to new NDF entry in the PCB

*.

*  Set an initial value for the PLACE argument.
      PLACE = NDF__NOPL

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain a PCB entry for the new NDF and export the required
*  placeholder.
      CALL NDF1_PLFOR( LOC, NAME, IPCB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL NDF1_EXPPL( IPCB, PLACE, STATUS )

*  If an error occurred, then annul the PCB entry.
         IF ( STATUS .NE. SAI__OK ) CALL NDF1_ANNPL( .TRUE., IPCB,
     :                                               STATUS )
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_PLACE_ERR',
     :   'NDF_PLACE: Error obtaining an NDF placeholder.', STATUS )
         CALL NDF1_TRACE( 'NDF_PLACE', STATUS )
      END IF

      END
