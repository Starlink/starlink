      SUBROUTINE ARD1_LSM( NDIM, LBND, UBND, MSKSIZ, MASK, DONE, B,
     :                     LBEXTB, UBEXTB, LBINTB, UBINTB, STATUS )
*+
*  Name:
*     ARD1_LSM

*  Purpose:
*     Load the supplied mask onto the stack.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_LSM( NDIM, LBND, UBND, MSKSIZ, MASK, DONE, B, LBEXTB,
*                    UBEXTB, LBINTB, UBINTB, STATUS )

*  Description:
*     The values in the supplied mask are copied into array B. The
*     interior and exterior bounding boxes of the supplied mask are
*     found and returned so long as they have not already been found.

*  Arguments:
*     NDIM = INTEGER (Given)
*        The number of dimensions in each array.
*     LBND( NDIM ) = INTEGER (Given)
*        The lower pixel index bounds of each array.
*     UBND( NDIM ) = INTEGER (Given)
*        The upper pixel index bounds of each array.
*     MSKSIZ = INTEGER (Given)
*        The total number of elements in each array.
*     MASK( MSKSIZ ) = INTEGER (Given)
*        The supplied mask (in vector form). This should hold zero for
*        all exterior points, and a positive value for all interior
*        points.
*     DONE = LOGICAL (Given and Returned)
*        If DONE is supplied .FALSE., then the bounding boxes of the
*        mask are found and returned in the arrays LBEXTB, UBEXTB,
*        LBINTB, UBINTB. If DONE is supplied .TRUE., then the contents
*        of the above arrays are left unchanged. DONE is always returned
*        .TRUE..
*     B( MSKSIZ ) = INTEGER (Returned)
*        The stack array (in vector form). This is used as the first
*        operand in the ARD expression. If INDEX is non-zero, it will
*        be returned holding a copy of the supplied mask, and will be
*        returned holding zeros otherwise.
*     LBEXTB( NDIM ) = INTEGER (Returned)
*        The lower pixel bounds of the smallest box which contains all
*        exterior points in B. A value of VAL__MAXI for element 1 is
*        used to indicate an infinite box (in practice, a box equal to
*        the size of the mask), and a value of VAL__MINI for element 1
*        is used to indicate a zero sized box.
*     UBEXTB( NDIM ) = INTEGER (Returned)
*        The upper pixel bounds of the smallest box which contains all
*        exterior points in B.
*     LBINTB( NDIM ) = INTEGER (Returned)
*        The lower pixel bounds of the smallest box which contains all
*        interior points in B. A value of VAL__MAXI for element 1 is
*        used to indicate an infinite box, and a value of VAL__MINI for
*        element 1 is used to indicate a zero sized box.
*     UBINTB( NDIM ) = INTEGER (Returned)
*        The upper pixel bounds of the smallest box which contains all
*        interior points in B.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     28-FEB-1994 (DSB):
*        Original version.
*     26-JUN-2001 (DSB):
*        Modified for ARD version 2.0.
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
      INTEGER MASK( MSKSIZ )

*  Arguments Given and Returned:
      LOGICAL DONE

*  Arguments Returned:
      INTEGER B( MSKSIZ )
      INTEGER LBEXTB( NDIM )
      INTEGER UBEXTB( NDIM )
      INTEGER LBINTB( NDIM )
      INTEGER UBINTB( NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :        EHDIM,             ! Highest axis without any EXT. values
     :        EL,                ! Pixel count
     :        I,                 ! Dimension count
     :        IHDIM,             ! Highest axis without any INT. values
     :        IND,               ! Current Cartesian index
     :        MINDEX( ARD__MXDIM )! Cartesian indices of current pixel

      LOGICAL
     :        EXT( ARD__MXDIM),  ! Does an object contain EXT. values?
     :        INT( ARD__MXDIM)   ! Does an object contain INT. values?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

* If the bounding boxes have already been found, we can just simply copy
* the entire mask array to the B array.
      IF( DONE ) THEN
         DO I = 1, MSKSIZ
            B( I ) = MASK( I )
         END DO


*  If the bounding boxes have not yet been found, we will need to be a
*  bit cleverer in order to retain information about the N-dimensional
*  co-ordinates of each pixel in the supplied 1-dimensional vector
*  arrays.
      ELSE

*  Initialise flags for each axis. These are used to indicate whether or
*  not the current object of the corresponding dimensionality (pixel,
*  row, plane, cube, etc) contains any exterior and/or interior values.
*  The dimensionality of the object is one less than the corresponding
*  axis index (e.g. the 1 dimensional object, the row, corresponds to
*  axis no. 2). The initial values indicate that no exterior or
*  interior values have yet been found. At the same time, initialise
*  the bounds of the exterior and interior bounding boxes, and set the
*  Cartesian indices of the current pixel to the lower bound of the
*  mask.
         DO I = 1, NDIM
            EXT( I ) = .FALSE.
            INT( I ) = .FALSE.

            LBEXTB( I ) = VAL__MAXI
            UBEXTB( I ) = VAL__MINI
            LBINTB( I ) = VAL__MAXI
            UBINTB( I ) = VAL__MINI

            MINDEX( I ) = LBND( I )

         END DO

*  Initialise variables which holds the highest axis indices for which
*  no exterior and/or interior values have yet been found.
         EHDIM = NDIM
         IHDIM = NDIM

*  Loop round every element in the mask.
         DO EL = 1, MSKSIZ

*  Copy the current pixel.
            B( EL ) = MAX( 0, MASK( EL ) )

*  Store the Cartesian index on the first axis in a local scalar
*  variable to avoid time spent indexing the MINDEX array for it.
            IND = MINDEX( 1 )

*  If the current pixel is exterior, update the bounds of the exterior
*  bounding box on the first axis.
            IF( B( EL ) .LE. 0 ) THEN

               IF( IND .LT. LBEXTB( 1 ) ) LBEXTB( 1 ) = IND
               IF( IND .GT. UBEXTB( 1 ) ) UBEXTB( 1 ) = IND

*  Set flags to indicate that at least one exterior value is contained
*  in objects of all (except zero) dimensionality in which the current
*  pixel resides (i.e. the current row contains an exterior value, the
*  current plane contains an exterior value, the current cube contains
*  an exterior value, etc).  The object of zero dimensions (i.e. the
*  pixel itself) is excluded because the bounds for the corresponding
*  axis (axis 1) have already been updated (above) to take account of
*  the exterior value. The loop does not re-set flags already known to
*  be set (by virtue of the upper limit being set to EHDIM).
               DO I = 2, EHDIM
                  EXT( I ) = .TRUE.
               END DO

*  Indicate that exterior values have been found for objects of all
*  dimensionality except zero (i.e. axis 1). Zero is excluded because
*  the corresponding flag refers to the next pixel which has not yet
*  been checked.
               EHDIM = 1

*  Now do the same for interior values.
            ELSE

               IF( IND .LT. LBINTB( 1 ) ) LBINTB( 1 ) = IND
               IF( IND .GT. UBINTB( 1 ) ) UBINTB( 1 ) = IND

               DO I = 2, IHDIM
                  INT( I ) = .TRUE.
               END DO

               IHDIM = 1

            END IF

*  Increment the Cartesian index on the first axis.
            MINDEX( 1 ) = IND + 1

*  If the index is now larger than the corresponding upper bound of the
*  mask...
            I = 1
            DO WHILE( MINDEX( I ) .GT. UBND( I ) )

*  Reset the index to the corresponding lower bound.
               MINDEX( I ) = LBND( I )

*  If the last dimension has been reached, leave things as they are so
*  that the DO WHILE loop will exit.
               IF( I .LT. NDIM ) THEN

*  Increment the axis index.
                  I = I + 1

*  Save the current index value.
                  IND = MINDEX( I )

*  If the object corresponding to the new axis (i.e. row, plane, cube,
*  etc) contains any exterior values, update the bounds of the exterior
*  bounding box on the current axis.
                  IF( EXT( I ) ) THEN

                     IF( IND .LT. LBEXTB( I ) ) LBEXTB( I ) = IND
                     IF( IND .GT. UBEXTB( I ) ) UBEXTB( I ) = IND

*  Clear the flag for the next object.
                     EXT( I ) = .FALSE.

*  Indicate that the next value on this axis is not known to contain any
*  exterior values.
                     EHDIM = I

                  END IF

*  Do the same for interior values.
                  IF( INT( I ) ) THEN

                     IF( IND .LT. LBINTB( I ) ) LBINTB( I ) = IND
                     IF( IND .GT. UBINTB( I ) ) UBINTB( I ) = IND

                     INT( I ) = .FALSE.

                     IHDIM = I

                  END IF

*  Increment the Cartesian index on the current axis.
                  MINDEX( I ) = IND + 1

               END IF

            END DO

         END DO

*  If no exterior pixels were contained in the supplied mask, indicate
*  that the returned exterior bounding box is of zero size, and the
*  interior bounding box is of infinite size.
         IF( LBEXTB( 1 ) .EQ. VAL__MAXI ) THEN
            LBEXTB( 1 ) = VAL__MINI
            LBINTB( 1 ) = VAL__MAXI

*  Otherwise, if no interior pixels were contained in the supplied
*  mask, indicate that the returned interior bounding box is of zero
*  size, and the exterior bounding box is of infinite size.
         ELSE IF( LBINTB( 1 ) .EQ. VAL__MAXI ) THEN
            LBINTB( 1 ) = VAL__MINI
            LBEXTB( 1 ) = VAL__MAXI

         END IF

*  Indicate that the bounding boxes have been found.
         DONE = .TRUE.

      END IF

      END
