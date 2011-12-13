      SUBROUTINE ARD1_LINMP( MAP, FRM, DLBND, DUBND, LINEAR, WCSDAT,
     :                       STATUS )
*+
*  Name:
*     ARD1_LINMP

*  Purpose:
*     Check that a mapping is linear and return a matrix describing it.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_LINMP( MAP, FRM, DLBND, DUBND, LINEAR, WCSDAT, STATUS )

*  Description:
*     This routine checks to see if the supplied Mapping is linear,
*     except for an optional shift of origin.
*
*     Unit vectors along each of the axes in the Mapping's input coordinate
*     system are transformed using the Mapping. If the Mapping is linear,
*     these transformed vectors form the columns of the required matrix. To
*     test for linearity, several test points are transformed using the
*     supplied Mapping, and then also transformed using the candidate
*     matrix formed by the above process. If the two resulting positions are
*     more or less equal to each other, then the Mapping is considered to
*     be linear.
*
*     To account for a possible shift of origin, the origin of the Mappings
*     input Frame is transformed using the supplied Mapping, and this
*     position is subtracted from all other transformed positions.

*  Arguments:
*     MAP = INTEGER (Given)
*        A pointer to the Mapping to be checked.
*     FRM = INTEGER (Given)
*        A pointer to the Frame corresponding to the Mapping output axes.
*     DLBND( * ) = DOUBLE PRECISION (Given)
*        The lower bounds of the input domain.
*     DUBND( * ) = DOUBLE PRECISION (Given)
*        The upper bounds of the input domain.
*     LINEAR = LOGICAL (Returned)
*        Returned .TRUE. if the Mapping is linear and .FALSE. otherwise.
*     WCSDAT( * ) = DOUBLE PRECISION (Returned)
*        If the Mapping is linear, then WCSDAT is returned holding the
*        coefficients of the linear mapping from pixel to user coords.
*        Otherwise, wcsdat(1) holds a lower limit on the distance (within
*        the user coords) per pixel, and the other elements in WCSDAT are
*        not used. The supplied array should have at least NDIM*(NDIM+1)
*        elements.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
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
*     17-JUL-2001 (DSB):
*        Original version.
*     14-OCT-2008 (DSB):
*        Correct test on DPP.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST_ constants and functions
      INCLUDE 'ARD_ERR'          ! ARD error constants
      INCLUDE 'ARD_CONST'        ! ARD private constants

*  Global Variables:
      INCLUDE 'ARD_COM'          ! ARD common blocks
*        CMN_LINOK = LOGICAL (Read)
*           If .FALSE., then no not allow any Mapping to be considered
*           linear.

*  Arguments Given:
      INTEGER MAP
      INTEGER FRM
      DOUBLE PRECISION DLBND( * )
      DOUBLE PRECISION DUBND( * )

*  Arguments Returned:
      LOGICAL LINEAR
      DOUBLE PRECISION WCSDAT( * )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      REAL RAND                  ! Random number in range [0,1]
      EXTERNAL ARD1_INIT         ! initialize ARD common blocks

*  Local Constants:
      DOUBLE PRECISION EPS       ! Grid axis increment used to evaluate
      PARAMETER ( EPS = 1 )      ! pixel size.

      DOUBLE PRECISION MAXERR    ! Maximum allowable error at a test
      PARAMETER ( MAXERR = 0.5 ) ! point, in pixels.

*  Local Variables:
      DOUBLE PRECISION A( ARD__MXDIM ) ! Start position
      DOUBLE PRECISION B( ARD__MXDIM ) ! End position
      DOUBLE PRECISION DIST      ! User distance between points
      DOUBLE PRECISION DPP       ! User distance per pixel
      DOUBLE PRECISION IN( 3*ARD__MXDIM + 7, ARD__MXDIM ) ! Input positions
      DOUBLE PRECISION ORIG( ARD__MXDIM )                 ! Transformed origin
      DOUBLE PRECISION OUT( 3*ARD__MXDIM + 7, ARD__MXDIM )! Output positions
      INTEGER I                  ! Loop count
      INTEGER ITEST              ! Loop count
      INTEGER J                  ! Loop count
      INTEGER K                  ! Loop count
      INTEGER NIN                ! No. of input axes
      INTEGER NOUT               ! No. of output axes
      INTEGER NTEST              ! No. of test positions tried so far
*.

      LINEAR = .FALSE.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store the number of input and output axes which the Mapping has.
      NIN = AST_GETI( MAP, 'NIN', STATUS )
      NOUT = AST_GETI( MAP, 'NOUT', STATUS )

*  The Mapping is not considered to be linear if the number of inputs and
*  outputs differ.
      IF( NIN .NE. NOUT ) GO TO 999

*  We only need to do the check if CMN_LINOK is supplied .TRUE.
      IF( CMN_LINOK ) THEN

*  Store unit vectors along each of the input axes, plus the origin.
         DO I = 1, NIN
            DO J = 1, NIN + 1
               IN( J, I ) = 0.0
            END DO
            IN( I, I ) = 1.0
         END DO

*  Also store the test points to be used, in pairs; the first in each pair
*  is a test point, the second is the same point but incremented by a small
*  amount on each axis. The first NIN test points are at the upper bound of
*  each of the NIN axes. The next test point is the lower bounds on all
*  axes. The next is the upper bounds on all axes. The next is the centre
*  of the array.
         DO ITEST = 1, NIN
            DO I = 1, NIN
               IN( NIN + 2*ITEST, I ) = DLBND( I )
            END DO
            IN( NIN + 2*ITEST, ITEST ) = DUBND( ITEST )
         END DO

         DO I = 1, NIN
            IN( 3*NIN + 2, I ) = DLBND( I )
            IN( 3*NIN + 4, I ) = DUBND( I )
            IN( 3*NIN + 6, I ) = 0.5*( DLBND( I ) + DUBND( I ) )
         END DO

         DO ITEST = 1, NIN + 3
            DO I = 1, NIN
               IN( NIN + 2*ITEST + 1, I ) = IN( NIN + 2*ITEST, I ) + EPS
            END DO
         END DO

*  Transform these vectors using the supplied Mapping. If the Mapping is
*  linear, the transformed data will define the required matrix, with three
*  extra columns holding the transformed extra points.
         CALL AST_TRANN( MAP, 3*NIN + 7, NIN, 3*ARD__MXDIM + 7, IN,
     :                   .TRUE., NOUT, 3*ARD__MXDIM + 7, OUT, STATUS )

*  Normalize the positions.
         DO I = 1, 3*NIN + 7
            DO J = 1, NOUT
               A( J ) = OUT( I, J )
            END DO
            CALL AST_NORM( FRM, A, STATUS )
            DO J = 1, NOUT
               OUT( I, J ) = A( J )
            END DO
         END DO

*  Find the minimum distance per pixel at any of the test points. If any
*  bad transformed points are encountered, the Mapping cannot be linear so
*  abort.
         DPP = AST__BAD
         DO ITEST = 1, NIN + 3

            DO I = 1, NOUT
               A( I ) = OUT( NIN + 2*ITEST, I )
               B( I ) = OUT( NIN + 2*ITEST + 1, I )
            END DO

            DIST = AST_DISTANCE( FRM, A, B, STATUS )
            IF( DIST .EQ. AST__BAD ) GO TO 999

            IF( ITEST .EQ. 1 ) THEN
               DPP = DIST / ( EPS*SQRT( DBLE( NIN ) ) )
            ELSE
               DPP = MIN( DPP, DIST / ( EPS*SQRT( DBLE( NIN ) ) ) )
            END IF

         END DO

*  Subtract the transformed origin (the first extra position) from each of
*  the first NIN positions.
         DO I = 1, NOUT
            ORIG( I ) = OUT( NIN + 1, I )
            DO J = 1, NIN
               OUT( J, I ) = OUT( J, I ) - ORIG( I )
            END DO
         END DO

*  We now test the Mapping for linearity. The linearity test must be passed
*  at all test points. Assume success to begin with.
         LINEAR = .TRUE.
         DO ITEST = 1, NIN + 3

*  The current test point is mapped using the candidate matrix.
            DO I = 1, NOUT
               A( I ) = ORIG( I )
               DO J = 1, NIN
                  A( I ) = A( I ) + OUT( J, I )*IN( NIN + 2*ITEST, J )
               END DO
               B( I ) = A( I )
            END DO

            CALL AST_NORM( FRM, A, STATUS )
            DO I = 1, NOUT
               IF( A( I ) .NE. B( I ) ) THEN
                  LINEAR = .FALSE.
                  GO TO 999
               END IF
               B( I ) = OUT( NIN + 2*ITEST, I )
            END DO

*  If the arc distance between the point found above, and the same
*  point found using the Mapping directly, is greater than the maximum error
*  allowed, the Mapping is considered not to be linear.
            IF( AST_DISTANCE( FRM, A, B, STATUS ) .GT. DPP*MAXERR ) THEN
               LINEAR = .FALSE.
               GO TO 999
            END IF

         END DO

*  If the linear mappings are not allowed, we do not yet have an estimate
*  of the distance per pixel.
      ELSE
         DPP = AST__BAD
      END IF

*  Arrive here if an error occurs, or the Mapping is non-linear.
 999  CONTINUE

*  If the mapping is linear, copy it to the returned array, row by row.
      IF( LINEAR ) THEN
         K = 1
         DO I = 1, NOUT
            WCSDAT( K ) = ORIG( I )
            K = K + 1
            DO J = 1, NIN
               WCSDAT( K ) = OUT( J, I )
               K = K + 1
            END DO
         END DO

*  If the Mapping is not linear, we need to return an estimate of the distance
*  (within the user coords) per pixel.
      ELSE

*  If we have not yet been able to estimate the maximum acceptable squared
*  error, we need to test some more points.
         NTEST = 0
         DO WHILE( DPP .EQ. AST__BAD .AND. STATUS .EQ. SAI__OK )

*  Choose a test point at random. Abort if more than 100 test points fail.
            IF( NTEST .GT. 100 .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = ARD__NOTST
               CALL ERR_REP( 'ARD1_LINMP_ERR1', 'Cannot find any '//
     :                       'pixels with good user coordinates.',
     :                       STATUS )
            ELSE
               NTEST = NTEST + 1
               DO I = 1, NIN
                  IN( 1, I )  = DLBND( I ) + DBLE( RAND( 0 ) )*
     :                                     ( DUBND( I ) - DLBND( I ) )
                  IN( 2, I )  = IN( 1, I ) + EPS
               END DO
            END IF

*  Transform it and normalize it.
            CALL AST_TRANN( MAP, 2, NIN, 3*ARD__MXDIM + 7, IN, .TRUE.,
     :                      NOUT, 3*ARD__MXDIM + 7, OUT, STATUS )

*  Find the distance per pixel between the transformed test point and its
*  neighbour.
            DO I = 1, NOUT
               A( I ) = OUT( 1, I )
               B( I ) = OUT( 2, I )
            END DO
            DPP = AST_DISTANCE( FRM, A, B, STATUS )

         END DO

*  Store an estimate of the arc-distance (within the user coords) per pixel.
         WCSDAT( 1 ) = DPP/( EPS*SQRT( DBLE( NIN ) ) )

      END IF

      END
