      SUBROUTINE ARY_LOC( IARY, LOC, STATUS )
*+
*  Name:
*     ARY_LOC

*  Purpose:
*     Obtain an HDS locator for an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_LOC( IARY, LOC, STATUS )

*  Description:
*     The routine returns an HDS locator for the data object referred to
*     by the supplied ARY identifier.

*  Arguments:
*     IARY = INTEGER (Given)
*        Array identifier.
*     LOC = CHARACTER * ( DAT__SZLOC ) (Returned)
*        The HDS locator. It should be annulled using DAT_ANNUL when no
*        longer needed. A value of DAT__NOLOC will be returned if an
*        error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
*     DSB: David S Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     20-OCT-2010 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_LOC( ACB_MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator for the data object.

      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_IDCB( ARY__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER IARY

*  Arguments Returned:
      CHARACTER * ( DAT__SZLOC ) LOC

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      INTEGER IACB               ! Index to ACB entry
      INTEGER IDCB               ! Index to data object entry in the DCB

*.

*  Initialise
      LOC = DAT__NOLOC

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the array identifier.
      CALL ARY1_IMPID( IARY, IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Get the DCB index for the data object.
         IDCB = ACB_IDCB( IACB )

*  Clone the data object locator.
         CALL DAT_CLONE( DCB_LOC( IDCB ), LOC, STATUS )

      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARY_LOC_ERR', 'ARY_LOC: Error obtaining an '//
     :                 'HDS locator for an array.', STATUS )
         CALL ARY1_TRACE( 'ARY_LOC', STATUS )
      END IF

      END
