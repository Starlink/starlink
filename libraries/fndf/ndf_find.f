      SUBROUTINE NDF_FIND( LOC, NAME, INDF, STATUS )
*+
*  Name:
*     NDF_FIND

*  Purpose:
*     Find an NDF and import it into the NDF_ system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_FIND( LOC, NAME, INDF, STATUS )

*  Description:
*     The routine finds an NDF within an HDS structure or container
*     file, imports it into the NDF_ system and issues an identifier
*     for it. The imported NDF may then be manipulated by the NDF_
*     routines.

*  Arguments:
*     LOC = CHARACTER * ( * ) (Given)
*        Locator to the enclosing HDS structure.
*     NAME = CHARACTER * ( * ) (Given)
*        Name of the HDS structure component to be imported.
*     INDF = INTEGER (Returned)
*        NDF identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The value given for the NAME argument may be an HDS path name,
*     consisting of several fields separated by '.', so that an NDF can
*     be found in a sub-component (or a sub-sub-component...) of the
*     structure identified by the locator LOC.  Array subscripts may
*     also be used in this component name.  Thus a string such as
*     'MYSTRUC.ZONE(2).IMAGE' could be used as a valid NAME value.
*     -  An NDF can be accessed within an explicitly named container
*     file by supplying the symbolic value DAT__ROOT for the LOC
*     argument and specifying the container file within the value
*     supplied for the NAME argument. Only READ access is available to
*     an NDF accessed in this way (for other modes of access, see the
*     NDF_OPEN routine).
*     -  If a blank value is given for the NAME argument, then the NDF
*     to be imported will be the object identified directly by the
*     locator LOC.
*     -  The locator supplied as input to this routine may later be
*     annulled without affecting the behaviour of the NDF_ system.
*     -  If this routine is called with STATUS set, then a value of
*     NDF__NOID will be returned for the INDF argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason.
*     -  The NDF__NOID constant is defined in the include file NDF_PAR.
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: D.S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-SEP-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     11-DEC-1990 (RFWS):
*        Changed to allow a compound or blank component name.
*     21-JUN-1993 (DSB):
*        Changed to allow container files to be included in the supplied
*        name.
*     12-AUG-1993 (RFWS):
*        Modified to export an NDF identifier explicitly, rather than
*        depending on the internal routine NDF1_NFIND to do this.
*     2-NOV-1993 (RFWS):
*        Updated to support foreign format files.
*     15-MAY-1995 (RFWS):
*        Fixed bug: missing status test was causing ACB identifier to be
*        annulled all the time.
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
      INTEGER INDF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to NDF entry in the ACB

*.

*  Set an initial value for the INDF argument.
      INDF = NDF__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find and import the NDF, assuming READ access if a root locator is
*  supplied.
      CALL NDF1_OPFOR( LOC, NAME, 'READ', IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Export an identifier for the new NDF.
         CALL NDF1_EXPID( IACB, INDF, STATUS )

*  If an error occurs, then annul the ACB entry.
         IF ( STATUS .NE. SAI__OK ) CALL NDF1_ANL( IACB, STATUS )
      END IF

*  If an error occurred, then reset the INDF argument, report context
*  information and call the error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         INDF = NDF__NOID
         CALL ERR_REP( 'NDF_FIND_ERR',
     :   'NDF_FIND: Error finding an NDF and importing it into the ' //
     :   'NDF_ system.', STATUS )
         CALL NDF1_TRACE( 'NDF_FIND', STATUS )
      END IF

      END
