      SUBROUTINE KPS1_PRFSM( NDIM, DIM, INDAT, VAR, INVAR, NP, POS,
     :                       OUTDAT, OUTVAR, BADD, BADV, STATUS )
*+
*  Name:
*     KPS1_PRFSM

*  Purpose:
*     Sample data arrays at a series of positions to create a 1-D profile.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_PRFSM( NDIM, DIM, INDAT, VAR, INVAR, NP, POS, OUTDAT,
*                      OUTVAR, BADD, BADV, STATUS )

*  Description:
*     This routine produces a 1-D array of data and variances values by
*     sampling the supplied data at given positions. Nearest neighbour
*     interpolation is used.

*  Arguments:
*     NDIM = INTEGER (Given)
*        The number of dimensions in the supplied arrays.
*     DIM( NDIM ) = INTEGER (Given)
*        The size of each dimension in the supplied data arrays.
*     INDAT( * ) = DOUBLE PRECISION (Given)
*        The input data array. Its size should equal the product of the
*        values supplied in DIM.
*     VAR = LOGICAL (Given)
*        Are input variances available?
*     INVAR( * ) = DOUBLE PRECISION (Given)
*        The input variance array. Its size should equal the product of the
*        values supplied in DIM. Only accessed if VAR is .TRUE.
*     NP = INTEGER (Given)
*        The number of samples required.
*     POS( NP, NDIM ) = DOUBLE PRECISION (Given)
*        The co-ordinates at the required sample positions.
*     OUTDAT( NP ) = DOUBLE PRECISION (Returned)
*        The sampled data values.
*     OUTVAR( NP ) = DOUBLE PRECISION (Returned)
*        The sampled variance values. Only accessed if VAR is .TRUE.
*     BADD = LOGICAL (Returned)
*        Were any bad values included in the returned profile data array?
*     BADV = LOGICAL (Returned)
*        Were any bad values included in the returned profile variance data?
*     STATUS = INTEGER (Given)
*        Global status value.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-SEP-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      INTEGER NDIM
      INTEGER DIM( NDIM )
      DOUBLE PRECISION INDAT( * )
      LOGICAL VAR
      DOUBLE PRECISION INVAR( * )
      INTEGER NP
      DOUBLE PRECISION POS( NP, NDIM )

*  Arguments Returned:
      DOUBLE PRECISION OUTDAT( NP )
      DOUBLE PRECISION OUTVAR( NP )
      LOGICAL BADD
      LOGICAL BADV

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Sample index
      INTEGER J                  ! Axis index
      INTEGER SKIP               ! Increment between values on current axis
      INTEGER IV                 ! Vector index of nearest pixel
      INTEGER IA                 ! Nearest integer value on current axis
*.

*  Initialise.
      BADD = .FALSE.
      BADV = .FALSE.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop round each sample.
      DO I = 1, NP

*  Find the vector index of the nearest pixel in the supplied array.
*  First initialise it.
         IV = 1

*  Indicate that moving from one pixel to the next on the current (first)
*  axis is achieved by increasing the vector index by 1.
         SKIP = 1

*  Go round each axis.
         DO J = 1, NDIM

*  If this axis is bad, store bad values in the returned arrays and leave
*  the loop, unless the axis spans only a single pixel in which case use
*  that pixel.
            IF( POS( I, J ) .EQ. AST__BAD ) THEN
               IF( DIM( J ) .GT. 1 ) THEN
                  OUTDAT( I ) = VAL__BADD
                  BADD = .TRUE.

                  IF( VAR ) THEN
                     OUTVAR( I ) = VAL__BADD
                     BADV = .TRUE.
                  END IF

                  GO TO 10
               ELSE
                  IA = 1
               END IF

*  Otherwise, get the index of the nearest integer value on this axis.
            ELSE
               IA = NINT( POS( I, J ) )
            END IF

*  If this is outside the bounds of the axis, store bad values in the
*  returned arrays, and leave the loop.
            IF( IA .LT. 1 .OR. IA .GT. DIM( J ) ) THEN
               OUTDAT( I ) = VAL__BADD
               BADD = .TRUE.

               IF( VAR ) THEN
                  OUTVAR( I ) = VAL__BADD
                  BADV = .TRUE.
               END IF

               GO TO 10

*  Otherwise, increment the vector index appropriately.
            ELSE
              IV = IV + ( IA - 1 )*SKIP
            END IF

*  Set the skip for the next axis.
            SKIP = SKIP * DIM( J )

         END DO

*  Store the data value.
         OUTDAT( I ) = INDAT( IV )
         IF( OUTDAT( I ) .EQ. VAL__BADD ) BADD = .TRUE.

*  If required, store the variance value.
         IF( VAR ) THEN
            OUTVAR( I ) = INVAR( IV )
            IF( OUTVAR( I ) .EQ. VAL__BADD ) BADV = .TRUE.
         END IF

 10      CONTINUE

      END DO

      END
