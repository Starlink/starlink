      SUBROUTINE ARY_SIZE( IARY, NPIX, STATUS )
*+
*  Name:
*     ARY_SIZE

*  Purpose:
*     Determine the size of an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_SIZE( IARY, NPIX, STATUS )

*  Description:
*     The routine returns the number of pixels in the array whose
*     identifier is supplied (i.e. the product of its dimensions).

*  Arguments:
*     IARY = INTEGER (Given)
*        Array identifier.
*     NPIX = INTEGER (Returned)
*        Number of pixels in the array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Import the array identifier.
*     -  Calculate the number of array elements from the bounds
*     information in the Access Control Block.
*     -  If an error occurred, then report context information.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-JUL-1989 (RFWS):
*        Original version.
*     7-MAR-1990 (RFWS):
*        Renamed SIZE argument to NPIX.
*     {enter_further_changes_here}

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
*        ACB_LBND( ARY__MXACB, ARY__MXACB ) = INTEGER (Read)
*           Lower bounds of array.
*        ACB_NDIM( ARY__MXACB ) = INTEGER (Read)
*           Number of array dimensions.
*        ACB_UBND( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read)
*           Upper bounds of array.

*  Arguments Given:
      INTEGER IARY

*  Arguments Returned:
      INTEGER NPIX

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to array entry in the ACB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the array identifier.
      CALL ARY1_IMPID( IARY, IACB, STATUS )

*  Calculate the number of array elements from the bounds information in
*  the ACB.
      CALL ARY1_NEL( ACB_NDIM( IACB ),
     :               ACB_LBND( 1, IACB ), ACB_UBND( 1, IACB ), NPIX,
     :               STATUS )
       
*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARY_SIZE_ERR',
     :   'ARY_SIZE: Error obtaining array size information.', STATUS )
         CALL ARY1_TRACE( 'ARY_SIZE', STATUS )
      END IF

      END
