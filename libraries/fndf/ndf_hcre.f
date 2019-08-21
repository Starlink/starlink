      SUBROUTINE NDF_HCRE( INDF, STATUS )
*+
*  Name:
*     NDF_HCRE

*  Purpose:
*     Ensure that a history component exists for an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_HCRE( INDF, STATUS )

*  Description:
*     The routine ensures that an NDF has a history component, creating
*     a new one if necessary. No action is taken if a history component
*     already exists.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     A history component may be removed from an NDF by calling
*     NDF_RESET with a component name of 'History'.

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
*     5-MAY-1993 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public contstants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants

*  Global Variables:
      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER INDF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to the NDF entry in the ACB
      INTEGER IDCB               ! Index to the data object in the DCB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that WRITE access to the NDF is available.
         CALL NDF1_CHACC( IACB, 'WRITE', STATUS )

*  Obtain an index to the NDF entry in the DCB and ensure that a
*  history component exists.
         IDCB = ACB_IDCB( IACB )
         CALL NDF1_HDCRE( IDCB, STATUS )
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_HCRE_ERR',
     :   'NDF_HCRE: Error ensuring that a history component exists ' //
     :   'for an NDF.', STATUS )
         CALL NDF1_TRACE( 'NDF_HCRE', STATUS )
      END IF

      END
