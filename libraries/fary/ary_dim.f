      SUBROUTINE ARY_DIM( IARY, NDIMX, DIM, NDIM, STATUS )
*+
*  Name:
*     ARY_DIM

*  Purpose:
*     Enquire the dimension sizes of an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_DIM( IARY, NDIMX, DIM, NDIM, STATUS )

*  Description:
*     The routine returns the size in pixels of each dimension of an
*     array, together with the total number of dimensions (the size of
*     a dimension is the difference between that dimension's upper and
*     lower pixel-index bounds + 1).

*  Arguments:
*     IARY = INTEGER (Given)
*        Array identifier.
*     NDIMX = INTEGER (Given)
*        Maximum number of dimension sizes to return (i.e. the declared
*        size of the DIM argument).
*     DIM( NDIMX ) = INTEGER (Returned)
*        Size of each dimension in pixels.
*     NDIM = INTEGER (Returned)
*        Total number of array dimensions.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If the array has fewer than NDIMX dimensions, then any
*     remaining elements of the DIM argument will be filled with 1's.
*     -  If the array has more than NDIMX dimensions, then the NDIM
*     argument will return the actual number of dimensions. In this
*     case only the first NDIMX dimension sizes will be returned, and
*     an error will result if the size of any of the excluded
*     dimensions exceeds 1.
*     -  The symbolic constant ARY__MXDIM may be used to declare the
*     size of the DIM argument so that it will be able to hold the
*     maximum number of array dimension sizes that this routine can
*     return. This constant is defined in the include file ARY_PAR.

*  Algorithm:
*     -  Import the array identifier.
*     -  Return the number of array dimensions from the Access Control
*     Block.
*     -  Return as many dimension size values as possible.
*     -  If any elements of the DIM argument remain unused, then fill
*     them with 1's.
*     -  If not all the dimension sizes could be returned, then check
*     that the size of any excluded dimensions does not exceed 1.
*     Report an error if any dimension size does.
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
*     5-MAR-1990 (RFWS):
*        Added code to pad dimension sizes with 1's and to check for
*        significant excluded dimensions.
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
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_LOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Data object locator.

      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_IDCB( ARY__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.
*        ACB_LBND( ARY__MXACB, ARY__MXACB ) = INTEGER (Read)
*           Lower bounds of array.
*        ACB_NDIM( ARY__MXACB ) = INTEGER (Read)
*           Number of array dimensions.
*        ACB_UBND( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read)
*           Upper bounds of array.

*  Arguments Given:
      INTEGER IARY
      INTEGER NDIMX

*  Arguments Returned:
      INTEGER DIM( NDIMX )
      INTEGER NDIM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter for dimensions
      INTEGER IACB               ! Index to array entry in the ACB
      INTEGER IDCB               ! Index to data object entry in the DCB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the array identifier.
      CALL ARY1_IMPID( IARY, IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Return the number of array dimensions.
         NDIM = ACB_NDIM( IACB )

*  Return as many dimension size values as possible.
         DO 1 I = 1, MIN( NDIMX, NDIM )
            DIM( I ) = ACB_UBND( I, IACB ) - ACB_LBND( I, IACB ) + 1
1        CONTINUE

*  Pad any remaining elements of DIM with 1's.
         DO 2 I = MIN( NDIMX, NDIM ) + 1, NDIMX
            DIM( I ) = 1
2        CONTINUE

*  If not all the dimensions were returned, then check to see whether
*  any excluded dimensions have sizes greater than 1. Report an error
*  if any excluded dimension does.
         DO 3 I = MIN( NDIMX, NDIM) + 1, NDIM
            IF ( ACB_UBND( I, IACB ) .NE. ACB_LBND( I, IACB ) ) THEN
               STATUS = ARY__XSDIM
               IDCB = ACB_IDCB( IACB )
               CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
               CALL MSG_SETI( 'NDIMX', NDIMX )
               CALL ERR_REP( 'ARY_DIM_NDIM',
     :         'The array structure ^ARRAY has more than ^NDIMX ' //
     :         'significant dimension(s).', STATUS )
               GO TO 4
            END IF
3        CONTINUE
4        CONTINUE
      END IF
       
*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARY_DIM_ERR',
     :   'ARY_DIM: Error obtaining array dimension size information.',
     :   STATUS )
         CALL ARY1_TRACE( 'ARY_DIM', STATUS )
      END IF

      END
