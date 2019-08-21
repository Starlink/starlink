      SUBROUTINE NDF_IMPRT( LOC, INDF, STATUS )
*+
*  Name:
*     NDF_IMPRT

*  Purpose:
*     Import an NDF into the NDF_ system from HDS.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_IMPRT( LOC, INDF, STATUS )

*  Description:
*     The routine imports an NDF into the NDF_ system from HDS and
*     issues an identifier for it. The NDF may then be manipulated by
*     the NDF_ routines.

*  Arguments:
*     LOC = CHARACTER * ( * ) (Given)
*        HDS locator to an NDF structure.
*     INDF = INTEGER (Returned)
*        NDF identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  THIS ROUTINE IS OBSOLETE. The same effect can be obtained by
*     calling NDF_FIND with its second (NAME) argument set to a blank
*     string.

*  Copyright:
*     Copyright (C) 1994 Particle Physics & Astronomy Research Council

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
*     26-SEP-1989 (RFWS):
*        Original, derived from the equivalent ARY_ routine.
*     26-SEP-1989 (RFWS):
*        Improved error message.
*     28-JUL-1994 (RFWS):
*        Documented as obsolete.
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

*  Arguments Given:
      CHARACTER * ( * ) LOC

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

*  Import the NDF structure into the ACB.
      CALL NDF1_IMP( LOC, IACB, STATUS )

*  Export an identifier for the new NDF.
      CALL NDF1_EXPID( IACB, INDF, STATUS )

*  If an error occurred, then reset the INDF value and report context
*  information.
      IF ( STATUS .NE. SAI__OK ) THEN
         INDF = NDF__NOID
         CALL ERR_REP( 'NDF_IMPRT_ERR',
     :   'NDF_IMPRT: Error importing an NDF into the NDF_ system ' //
     :   'from HDS.', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF_IMPRT', STATUS )

      END
