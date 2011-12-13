      SUBROUTINE ARY_NDIM( IARY, NDIM, STATUS )
*+
*  Name:
*     ARY_NDIM

*  Purpose:
*     Enquire the dimensionality of an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_NDIM( IARY, NDIM, STATUS )

*  Description:
*     The routine determines the number of dimensions which an array
*     has.

*  Arguments:
*     IARY = INTEGER (Given)
*        Array identifier.
*     NDIM = INTEGER (Returned)
*        Number of array dimensions.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Import the array identifier.
*     -  Return the number of array dimensions.
*     -  If an error occurred, then report context information.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     1-NOV-1990 (RFWS):
*        Original version, derived from the ARY_DIM routine.
*     {enter_changes_here}

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

*  Global Variables:
      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_NDIM( ARY__MXACB ) = INTEGER (Read)
*           Number of array dimensions.

*  Arguments Given:
      INTEGER IARY

*  Arguments Returned:
      INTEGER NDIM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to array entry in the ACB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the array identifier.
      CALL ARY1_IMPID( IARY, IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Return the number of array dimensions.
         NDIM = ACB_NDIM( IACB )
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARY_NDIM_ERR',
     :   'ARY_NDIM: Error determining the dimensionality of an array.',
     :   STATUS )
         CALL ARY1_TRACE( 'ARY_NDIM', STATUS )
      END IF

      END
