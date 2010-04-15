      SUBROUTINE ARY_CMPLX( IARY, CMPLX, STATUS )
*+
*  Name:
*     ARY_CMPLX

*  Purpose:
*     Determine whether an array holds complex values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_CMPLX( IARY, CMPLX, STATUS )

*  Description:
*     The routine returns a logical value indicating whether an array
*     holds complex values.

*  Arguments:
*     IARY = INTEGER (Given)
*        Array identifier.
*     CMPLX = LOGICAL (Returned)
*        Whether the array holds complex values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Import the array identifier.
*     -  Obtain the Data Control Block index for the data object.
*     -  Ensure that data type information is available for the data
*     object.
*     -  Assign the result.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-JUN-1989 (RFWS):
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
*        DCB_CPX( ACB_MXDCB ) = LOGICAL (Read)
*           Whether data object is complex.

      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_IDCB( ARY__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER IARY

*  Arguments Returned:
      LOGICAL CMPLX

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      INTEGER IACB               ! Index to ACB entry
      INTEGER IDCB               ! Index to data object entry in the DCB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the array identifier.
      CALL ARY1_IMPID( IARY, IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Get the DCB index for the data object.
         IDCB = ACB_IDCB( IACB )

*  Ensure that data type information is available.
         CALL ARY1_DTYP( IDCB, STATUS )

*  Assign the result.
         CMPLX = DCB_CPX( IDCB )
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARY_CMPLX_ERR',
     :   'ARY_CMPLX: Error determining if an array holds complex ' //
     :   'values.', STATUS )
         CALL ARY1_TRACE( 'ARY_CMPLX', STATUS )
      END IF

      END
