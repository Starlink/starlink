      SUBROUTINE NDF_VALID( INDF, VALID, STATUS )
*+
*  Name:
*     NDF_VALID

*  Purpose:
*     Determine whether an NDF identifier is valid.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_VALID( INDF, VALID, STATUS )

*  Description:
*     The routine determines whether an NDF identifier is valid (i.e.
*     associated with an NDF).

*  Arguments:
*     INDF = INTEGER (Given)
*        Identifier to be tested.
*     VALID = LOGICAL (Returned)
*        Whether the identifier is valid.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Try to convert the identifier into the associated ACB index.
*     -  Note whether the attempt succeeded or not.

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
*     29-SEP-1989 (RFWS):
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

*  Arguments Given:
      INTEGER INDF

*  Arguments Returned:
      LOGICAL VALID

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to associated ACB entry

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Try to convert the identifier into an ACB index.
      CALL NDF1_ID2AC( INDF, IACB )

*  Note whether the attempt succeeded; it did not if a value of zero
*  was returned.
      VALID = IACB .NE. 0

      END
