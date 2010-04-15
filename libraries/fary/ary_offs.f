      SUBROUTINE ARY_OFFS( IARY1, IARY2, MXOFFS, OFFS, STATUS )
*+
*  Name:
*     ARY_OFFS

*  Purpose:
*     Obtain the pixel offset between two arrays.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_OFFS( IARY1, IARY2, MXOFFS, OFFS, STATUS )

*  Description:
*     The routine returns the pixel offset for each requested dimension
*     between two arrays. These values are the offsets which should be
*     added to the pixel indices of the first array to obtain the
*     indices of the corresponding pixel in the second array.

*  Arguments:
*     IARY1 = INTEGER (Given)
*        First array identifier.
*     IARY2 = INTEGER (Given)
*        Second array identifier.
*     MXOFFS = INTEGER (Given)
*        Maximum number of pixel offsets to return (i.e. the declared
*        size of the OFFS argument).
*     OFFS( MXOFFS ) = INTEGER (Returned)
*        Array of pixel offsets for each dimension.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The two array identifiers supplied need not refer to the same
*     base array (although they may often do so). If they do not, then
*     the offset between the pixels in each array is determined by
*     matching the pixel indices of their respective base arrays.
*     -  Note that non-zero pixel offsets may exist even for dimensions
*     which exceed the dimensionality of either of the two arrays
*     supplied. The symbolic constant ARY__MXDIM may be used to declare
*     the size of the OFFS argument so that it will be able to hold the
*     maximum number of non-zero offsets that this routine can return.

*  Algorithm:
*     -  Import the array identifiers.
*     -  Obtain indices to the data object entries in the DCB.
*     -  Loop to calculate the offset for each possible array dimension.
*     -  Combine the accumulated pixel-index shifts for each array and
*     its data object.
*     -  Pad remaining values with zero.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-OCT-1990 (RFWS):
*        Original version.
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
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_NDIM( ARY__MXDCB ) = INTEGER (Read)
*           Number of data object dimensions.
*        DCB_SFT( ARY__MXDIM, ARY__MXDCB ) = INTEGER (Read)
*           Accumulated pixel index shifts for each data object.

      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_IDCB( ARY__MXACB ) = INTEGER (Read)
*           Index to the data object entry in the DCB.
*        ACB_NDIM( ARY__MXACB ) = INTEGER (Read)
*           Number of array dimensions.
*        ACB_SFT( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read)
*           Accumulated pixel index shifts for the array's ACB entry.

*  Arguments Given:
      INTEGER IARY1
      INTEGER IARY2
      INTEGER MXOFFS

*  Arguments Returned:
      INTEGER OFFS( MXOFFS )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter for dimensions
      INTEGER IACB1              ! Index to first array ACB entry
      INTEGER IACB2              ! Index to second array ACB entry
      INTEGER IDCB1              ! Index to first array DCB entry
      INTEGER IDCB2              ! Index to second array DCB entry

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the array identifiers.
      CALL ARY1_IMPID( IARY1, IACB1, STATUS )
      CALL ARY1_IMPID( IARY2, IACB2, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain indices to the data object entries in the DCB.
         IDCB1 = ACB_IDCB( IACB1 )
         IDCB2 = ACB_IDCB( IACB2 )

*  Loop to calculate the offset for each possible array dimension.
         DO 1 I = 1, MIN( MXOFFS, ARY__MXDIM )

*  Combine the accumulated pixel-index shifts for each array and its
*  data object.
            OFFS( I ) = ( ACB_SFT( I, IACB2 ) - DCB_SFT( I, IDCB2 ) ) -
     :                  ( ACB_SFT( I, IACB1 ) - DCB_SFT( I, IDCB1 ) )
 1       CONTINUE

*  Pad remaining values with zero.
         DO 2 I = ARY__MXDIM + 1, MXOFFS
            OFFS( I ) = 0
 2       CONTINUE
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARY_OFFS_ERR',
     :   'ARY_OFFS: Error obtaining the pixel offset between two ' //
     :   'arrays.', STATUS )
         CALL ARY1_TRACE( 'ARY_OFFS', STATUS )
      END IF

      END
