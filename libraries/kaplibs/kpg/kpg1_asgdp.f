      SUBROUTINE KPG1_ASGDP( MAP, NDIM1, NDIM2, LBND, UBND, INPOS,
     :                       OUTPOS, STATUS )
*+
*  Name:
*     KPG1_ASGDP

*  Purpose:
*     Finds a position with good output co-ordinates within a given input
*     region of a supplied Mapping.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASGDP( MAP, NDIM1, NDIM2, LBND, UBND, INPOS, OUTPOS, STATUS )

*  Description:
*     This routine finds a position which has good co-ordinates in the
*     output Frame of the given Mapping, and is a significant distance
*     from the origin of both input and output Frame. It returns both the
*     input and output co-ordinates at this position. The position is
*     constrained to lie within a specified box within the input Frame.
*     The first point to be tested is the centre of the box. If this does
*     not pass the tests described above, a set of 10000 points randomly
*     distributed within the box is tested. An error is reported if no
*     good position can be found.

*  Arguments:
*     MAP = INTEGER (Given)
*        The Mapping to use.
*     NDIM1 = INTEGER (Given)
*        The number of input co-ordinates.
*     NDIM2 = INTEGER (Given)
*        The number of output co-ordinates.
*     LBND( NDIM1 ) = DOUBLE PRECISION (Given)
*        The lower bounds of the test box, within the input Frame of the
*        supplied Mapping.
*     UBND( NDIM1 ) = DOUBLE PRECISION (Given)
*        The upper bounds of the test box, within the input Frame of the
*        supplied Mapping.
*     INPOS( NDIM1 ) = DOUBLE PRECISION (Returned)
*        The returned input co-ordinates at the selected position.
*        There will be no AST__BAD values in this array.
*     OUTPOS( NDIM2 ) = DOUBLE PRECISION (Returned)
*        The returned output co-ordinates at the selected position.
*        There will be no AST__BAD values in this array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     31-AUG-2000 (DSB):
*        Original version.
*     23-AUG-2011 (DSB):
*        Ensure the returned point is at least 10% of the box width away
*        from the origin of both input and output Frame. This is because
*        the origin of a coordinate system is a bad test point as it has
*        unusual properties (e.g. multiplying the value zero by a scaling
*        factor does not change the value).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      INTEGER MAP
      INTEGER NDIM1
      INTEGER NDIM2
      DOUBLE PRECISION LBND( NDIM1 )
      DOUBLE PRECISION UBND( NDIM1 )

*  Arguments Returned:
      DOUBLE PRECISION INPOS( NDIM1 )
      DOUBLE PRECISION OUTPOS( NDIM2 )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      REAL SLA_RANDOM            ! Random-number generator
      REAL KPG1_SEED             ! Random-number seed initialisation

*  Local Constants:
      INTEGER NBATCH             ! Max. no. of batches of random points to try
      PARAMETER ( NBATCH = 100 )

      INTEGER SZBATCH            ! No. of points in one batch
      PARAMETER ( SZBATCH = 100 )

*  Local Variables:
      DOUBLE PRECISION EXIN( NDF__MXDIM )  ! Input exclusion zone
      DOUBLE PRECISION EXOUT( NDF__MXDIM ) ! Output exclusion zone
      DOUBLE PRECISION IN( SZBATCH, NDF__MXDIM )  ! Input positions
      DOUBLE PRECISION LBOUT     ! Lower output bounds
      DOUBLE PRECISION OUT( SZBATCH, NDF__MXDIM ) ! Output positions
      DOUBLE PRECISION UBOUT     ! Upper output bounds
      DOUBLE PRECISION XL( NDF__MXDIM ) ! Lower bounds input pos
      DOUBLE PRECISION XU( NDF__MXDIM ) ! Upper bounds input pos
      INTEGER I                  ! Batch index
      INTEGER J                  ! Sample index
      INTEGER K                  ! Axis index
      LOGICAL GOOD               ! Is this position good on all output axes?
      LOGICAL OK                 ! Is this position far from the origin?
      REAL SEED                  ! Random number generator seed
      REAL VALUE                 ! Random number
*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Get the range covered by the mapped box on each output axis.
      DO K = 1, NDIM2
         CALL AST_MAPBOX( MAP, LBND, UBND, .TRUE., K, LBOUT,
     :                    UBOUT, XL, XU, STATUS )

*  Set up the size of the exclusion zone around the origin of the
*  output coordinate system (10% of the box width).
         EXOUT( K ) = 0.1*( UBOUT - LBOUT )
      END DO

*  Set up the size of the exclusion zone around the origin of the
*  input coordinate system (10% of the box width).
      DO K = 1, NDIM1
         EXIN( K ) = 0.1*( UBND( K ) - LBND( K ) )
      END DO

*  If the centre of the input box is far enough away from the origin, we
*  consider using it first. Store the input co-ordinates.
      OK = .TRUE.
      DO K = 1, NDIM1
         INPOS( K ) = 0.5*( LBND( K ) + UBND( K ) )
         IF( ABS( INPOS( K ) ) .LE. EXIN( K ) ) OK = .FALSE.
      END DO

*  If the centre is far enough away from the origin, transform this point,
*  and see if all output co-ordinate values are good, and are
*  sufficiently far from the origin of the output coordinate system.
      IF( OK ) THEN
         CALL AST_TRANN( MAP, 1, NDIM1, 1, INPOS, .TRUE., NDIM2, 1,
     :                   OUTPOS, STATUS )
         GOOD = .TRUE.
         DO K = 1, NDIM2
           IF( OUTPOS( K ) .EQ. AST__BAD ) THEN
              GOOD = .FALSE.
           ELSE IF( ABS( OUTPOS( K ) ) .LE. EXOUT( K ) ) THEN
              GOOD = .FALSE.
           END IF
         END DO
      ELSE
         GOOD = .FALSE.
      END IF

*  If the point is not good...
      IF( .NOT. GOOD .AND. STATUS .EQ. SAI__OK ) THEN

*  Initialise the random-number generator seed.  It is taken as input
*  by SLA_RANDOM, which returns a pseudo-random number between 0 and
*  1, and is updated on return. Use SLA_RANDOM once here to fully
*  randomize numbers generated below.
         SEED = KPG1_SEED( STATUS )
         VALUE = SLA_RANDOM( SEED )

*  Loop round checking batches of positions for good points. We use
*  batches to 1) avoid the overhead of calling AST_TRANN on lots of
*  single positions, and 2) to avoid transforming all points at once
*  since we may not need anythign like NBATCH*SZBATCH points to find
*  a good position.
         DO I = 1, NBATCH

*  Set up a batch of points randomly distributed within the input box,
*  but exclude ones that are too close to the origin of the input
*  coordinate system.
            DO J = 1, SZBATCH
               OK = .FALSE.
               DO WHILE( .NOT. OK )
                  OK = .TRUE.
                  DO K = 1, NDIM1
                     IN( J, K ) = LBND( K ) + ( UBND( K ) -
     :                                LBND( K ) )*SLA_RANDOM( SEED )
                     IF( ABS( IN( J, K ) ) .LE. EXIN( K ) ) OK = .FALSE.
                  END DO
               END DO
            END DO

*  Transform them.
            CALL AST_TRANN( MAP, SZBATCH, NDIM1, SZBATCH, IN, .TRUE.,
     :                      NDIM2, SZBATCH, OUT, STATUS )

*  ABort if an error occurred.
            IF( STATUS .NE. SAI__OK ) GO TO 999

*  Loop round each transformed point.
            DO J = 1, SZBATCH

*  Check this point.
               GOOD = .TRUE.
               DO K = 1, NDIM2
                  IF( OUT( J, K ) .EQ. AST__BAD ) THEN
                     GOOD = .FALSE.
                  ELSE IF( ABS( OUT( J, K ) ) .LE. EXOUT( K ) ) THEN
                     GOOD = .FALSE.
                  END IF
               END DO

*  If it is good in the output Frame, store the returned positions and
*  break out of the loop.
               IF( GOOD ) THEN
                  DO K = 1, NDIM1
                     INPOS( K ) = IN( J, K )
                  END DO

                  DO K = 1, NDIM2
                     OUTPOS( K ) = OUT( J, K )
                  END DO

                  GO TO 999

               END IF

            END DO

         END DO

*  We only arrive here if we have not found a good position. So report an
*  error.
         IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'KPG1_ASGDP_ERR1', 'Cannot find any pixels '//
     :                    'with valid co-ordinates.', STATUS )
         END IF

      END IF

 999  CONTINUE

      END
