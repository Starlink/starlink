      SUBROUTINE NDF_ISACC( INDF, ACCESS, ISACC, STATUS )
*+
*  Name:
*     NDF_ISACC

*  Purpose:
*     Determine whether a specified type of NDF access is available.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_ISACC( INDF, ACCESS, ISACC, STATUS )

*  Description:
*     The routine determines whether a specified type of access to an
*     NDF is available, or whether it has been disabled. If access is
*     not available, then any attempt to access the NDF in this way
*     will fail.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     ACCESS = CHARACTER * ( * ) (Given)
*        The type of NDF access required: 'BOUNDS', 'DELETE', 'SHIFT',
*        'TYPE' or 'WRITE' (see the Notes section for details).
*     ISACC = LOGICAL (Returned)
*        Whether the specified type of access is available.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The valid access types control the following operations on the
*     NDF:
*     -  'BOUNDS' permits the pixel-index bounds of a base NDF to be
*     altered.
*     -  'DELETE' permits deletion of the NDF.
*     -  'SHIFT' permits pixel-index shifts to be applied to a base
*     NDF.
*     -  'TYPE' permits the data types of an NDF's components to be
*     altered.
*     -  'WRITE' permits new values to be written to the NDF, and the
*     state of any of its components to be reset.

*  Algorithm:
*     -  Import the NDF identifier.
*     -  Determine if the specified type of access is permitted.
*     -  If an error occurred, then report context information.

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
      CHARACTER * ( * ) ACCESS

*  Arguments Returned:
      LOGICAL ISACC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to the NDF entry in the ACB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Determine whether access is available.
      CALL NDF1_ACCOK( IACB, ACCESS, ISACC, STATUS )

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_ISACC_ERR',
     :   'NDF_ISACC: Error determining whether a specified type of ' //
     :   'NDF access is available.', STATUS )
         CALL NDF1_TRACE( 'NDF_ISACC', STATUS )
      END IF

      END
