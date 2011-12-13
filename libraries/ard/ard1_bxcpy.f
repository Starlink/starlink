      SUBROUTINE ARD1_BXCPY( NDIM, LBND, UBND, MSKSIZ, A, LBOX, UBOX,
     :                       B, STATUS )
*+
*  Name:
*     ARD1_BXCPY

*  Purpose:
*     Copy a box region from one array to another.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_BXCPY( NDIM, LBND, UBND, MSKSIZ, A, LBOX, UBOX, B,
*                      STATUS )

*  Description:
*     The area of array A within the supplied bounding box is copied to
*     the same area in array B. All elements of B outside the box are
*     left unchanged. A general N-d algorithm is used.

*  Arguments:
*     NDIM = INTEGER (Given)
*        The number of dimensions in each array.
*     LBND( NDIM ) = INTEGER (Given)
*        The lower pixel index bounds of each array.
*     UBND( NDIM ) = INTEGER (Given)
*        The upper pixel index bounds of each array.
*     MSKSIZ = INTEGER (Given)
*        The total number of elements in each array.
*     A( MSKSIZ ) = INTEGER (Given)
*        The input array (in vector form).
*     LBOX( NDIM ) = INTEGER (Given)
*        The lower pixel bounds of the box. A value of VAL__MAXI for
*        element 1 is used to indicate an infinite box, and a value of
*        VAL__MINI for element 1 is used to indicate a zero sized box.
*     UBOX( NDIM ) = INTEGER (Given)
*        The upper pixel bounds of the box.
*     B( MSKSIZ ) = INTEGER (Given and Returned)
*        The output array (in vector form).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-MAR-1994 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ARD_CONST'        ! ARD private constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants

*  Arguments Given:
      INTEGER NDIM
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )
      INTEGER MSKSIZ
      INTEGER A( MSKSIZ )
      INTEGER LBOX( NDIM )
      INTEGER UBOX( NDIM )

*  Arguments Given and Returned:
      INTEGER B( MSKSIZ )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :        BDIM,              ! A dimension size in the box
     :        BINDEX( ARD__MXDIM ),! Current Cartesian co-ordinates
     :        BOXEL,             ! Vector address within the box
     :        BOXSIZ,            ! No. of pixels in the box
     :        I,                 ! Loop count
     :        LLBOX( ARD__MXDIM ),! Supplied box limited to mask area
     :        LUBOX( ARD__MXDIM ),! Supplied box limited to mask area
     :        MDIM,              ! A dimension size in supplied array
     :        P,                 ! No. of pixels in (N-1)-Dim. object
     :        VA,                ! Vector address within supplied array
     :        VAINC( ARD__MXDIM) ! VA increment between N-D objects

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Deal first with the simple cases where the box is "infinite".
      IF( LBOX( 1 ) .EQ. VAL__MAXI ) THEN
         DO VA = 1, MSKSIZ
            B( VA ) = A( VA )
         END DO

*  If the box is null, return without doing anything.
      ELSE IF( LBOX( 1 ) .NE. VAL__MINI ) THEN

*  Working in a variable number of dimensions introduces complications
*  not met if the dimensionality is fixed; particularly, arrays cannot
*  be declared with the correct number of dimensions (since the number
*  is only known at run-time). They can therefore only be declared as
*  1-d vectors. The position of each pixel in the box found above can
*  be described in several ways;
*
*  1. By Cartesian pixel indices.
*
*  2. By a single vector address starting with 1 at the lower bounds
*  corner of the box and increasing to BOXSIZ at the upper bounds
*  corner (where BOXSIZ = total number of pixels in the box).
*
*  3. By a single vector address starting with 1 at the lower bounds
*  corner of the array B and increasing to MSKSIZ at the upper bounds
*  corner (where MSKSIZ = total number of pixels in array B).
*
*  The pixels forming the box will have continuus vector addresses
*  using system 2. above, but will have gaps using system 3.  The
*  first task is to establish constants needed to be able to transform
*  positions from system 2. to system 3. (only system 3. can be used to
*  actually access the arrays).
*
*  Initialise things.
         P = 1
         BOXSIZ = 1
         VA = 1

*  Loop round every dimension.
         DO I = 1, NDIM

*  Limit the USED box to the bounds of the mask.
            LLBOX( I ) = MAX( LBOX( I ), LBND( I ) )
            LUBOX( I ) = MIN( UBOX( I ), UBND( I ) )

*  If the box is null, return without doing anything.
            IF( LLBOX( I ) .GT. LUBOX( I ) ) GO TO 999

*  Set the Cartesian index of the first box pixel to be processed to the
*  lower box bound.
            BINDEX( I ) = LLBOX( I )

*  Store the size of the current box dimension, and include it as a
*  factor in the total number of pixels in the box.
            BDIM = LUBOX( I ) - LLBOX( I ) + 1
            BOXSIZ = BOXSIZ*BDIM

*  Update the B array vector address (system 3.) of the lower bounds
*  corner of the box.
            VA = VA + ( LLBOX( I ) - LBND( I ) )*P

*  Store the size of the current dimension of the input arrays.
            MDIM = UBND( I ) - LBND( I ) + 1

*  Store the increase in the B array vector address which is required to
*  move from the end of one row, plane, cube, etc (depending on the
*  current dimension) of the box, to the start of the next row, plane,
*  cube, etc.
            VAINC( I ) = P*( MDIM - BDIM )

*  Find the number of pixels included in a row,plane,cube,etc, of array
*  B.
            P = P*MDIM

         END DO

*  Loop round every element in the box. The Cartesian indices of the
*  current box pixel are contained in BINDEX. The B array vector address
*  (system 3.) of the current pixel is VA. The box vector address
*  (system 2.) of the current pixel is BOXEL.
         DO BOXEL = 1, BOXSIZ

*  Assign the value to the current pixel.
            B( VA ) = A( VA )

*  Increment the B array vector address of the next pixel to be
*  processed
            VA = VA + 1

*  Increment the Cartesian index on the first axis of the box.
            BINDEX( 1 ) = BINDEX( 1 ) + 1

*  If the index is now larger than the corresponding upper bound of the
*  box...
            I = 1
            DO WHILE( BINDEX( I ) .GT. LUBOX( I ) )

*  Reset the index to the corresponding lower bound.
               BINDEX( I ) = LLBOX( I )

*  If the last dimension has been reached, leave things as they are so
*  that the DO WHILE loop will exit.
               IF( I .LT. NDIM ) THEN

*  Increase the B array vector address to point to the start of the next
*  row, plane, cube, etc (depending on the current dimension) of the
*  box.
                  VA = VA + VAINC( I )

*  Increment the index for the next higher dimension.
                  I = I + 1
                  BINDEX( I ) = BINDEX( I ) + 1

               END IF

            END DO

         END DO

      END IF

 999  CONTINUE

      END
