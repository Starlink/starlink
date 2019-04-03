      SUBROUTINE NDF_ISBAS( INDF, ISBAS, STATUS )
*+
*  Name:
*     NDF_ISBAS

*  Purpose:
*     Enquire if an NDF is a base NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_ISBAS( INDF, ISBAS, STATUS )

*  Description:
*     The routine returns a logical value indicating whether the NDF
*     whose identifier is supplied is a base NDF (as opposed to an NDF
*     section).

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     ISBAS = LOGICAL (Returned)
*        Whether the NDF is a base NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Import the NDF identifier.
*     -  Determine whether the NDF is a base NDF from its Access Control
*     Block entry.
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
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants

*  Global Variables:
      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_CUT( NDF__MXACB ) = LOGICAL (Read)
*           Whether the NDF is a cut.

*  Arguments Given:
      INTEGER INDF

*  Arguments Returned:
      LOGICAL ISBAS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to NDF entry in the ACB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  The NDF is a base NDF if it is not a cut.
         ISBAS = .NOT. ACB_CUT( IACB )
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_ISBAS_ERR',
     :   'NDF_ISBAS: Error enquiring if an NDF is a base NDF.',
     :   STATUS )
         CALL NDF1_TRACE( 'NDF_ISBAS', STATUS )
      END IF

      END
