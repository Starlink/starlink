      SUBROUTINE NDF_CLONE( INDF1, INDF2, STATUS )
*+
*  Name:
*     NDF_CLONE

*  Purpose:
*     Clone an NDF identifier.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_CLONE( INDF1, INDF2, STATUS )

*  Description:
*     The routine produces a "cloned" copy of an NDF identifier (i.e.
*     it produces a new identifier describing an NDF with identical
*     attributes to the original).

*  Arguments:
*     INDF1 = INTEGER (Given)
*        NDF identifier to be cloned.
*     INDF2 = INTEGER (Returned)
*        Cloned identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If this routine is called with STATUS set, then a value of
*     NDF__NOID will be returned for the INDF2 argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason. The NDF__NOID
*     constant is defined in the include file NDF_PAR.

*  Algorithm:
*     -  Set an initial value of NDF__NOID for the INDF2 argument
*     before checking the inherited status.
*     -  Import the original NDF identifier.
*     -  Produce a cloned copy of its ACB entry.
*     -  Export an identifier for the new NDF.
*     -  If an error occurred, then reset the INDF2 argument to
*     NDF__NOID and report context information.

*  Copyright:
*     Copyright (C) 1989 Science & Engineering Research Council.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-OCT-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     {enter_changes_here}

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
      INTEGER INDF1

*  Arguments Returned:
      INTEGER INDF2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB1              ! Original NDF index in the ACB
      INTEGER IACB2              ! Cloned NDF index in the ACB

*.

*  Set an initial value for the INDF2 argument.
      INDF2 = NDF__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the original NDF identifier.
      CALL NDF1_IMPID( INDF1, IACB1, STATUS )

*  Produce a cloned copy of its ACB entry.
      CALL NDF1_CLN( IACB1, IACB2, STATUS )

*  Export an identifier for the new NDF.
      CALL NDF1_EXPID( IACB2, INDF2, STATUS )

*  If an error occurred, then reset the INDF2 argument to NDF__NOID,
*  report context information and call the error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         INDF2 = NDF__NOID
         CALL ERR_REP( 'NDF_CLONE_ERR',
     :   'NDF_CLONE: Error cloning an NDF identifier.', STATUS )
         CALL NDF1_TRACE( 'NDF_CLONE', STATUS )
      END IF

      END
