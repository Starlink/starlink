      SUBROUTINE IRA1_AITO( FORWRD, NVAL, C1, C2, STATUS )
*+
*  Name:
*     IRA1_AITO

*  Purpose:
*     Transform coordinate data using a Aitoff projection.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_AITO( FORWRD, NVAL, C1, C2, STATUS )

*  Description:
*     Applies a forward or inverse AITOFF projection to the supplied
*     coordinate data. If FORWRD is true (image to sky conversion), C1
*     and C2 should contain values of U and V on entry, and will hold
*     values of local longitude and latitue on exit. If FORWRD is false,
*     the opposite will be true. See routine IRA1_IPRJ for a description
*     of local coordinates and (U,V) coordinates.

*  Arguments:
*     FORWRD = LOGICAL (Given)
*        If true then the forward mapping is used from (U,V) coordinates
*        to local coordinates. Otherwise, the inverse mapping from local
*        coordinates to (U,V) coordinates is used.
*     NVAL = INTEGER (Given)
*        The number of coordinate points to be transformed.
*     C1( NVAL ) = DOUBLE PRECISION (Given and Returned)
*        If FORWRD is true, then C1 holds U values on entry and local
*        longitude values on exit.
*     C2( NVAL ) = DOUBLE PRECISION (Given)
*        If FORWRD is true, then C2 holds V values on entry and local
*        latitude values on exit.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1990, 1991 Science & Engineering Research Council.
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
*     18-DEC-1990 (DSB):
*        Original version.
*     9-APR-1991 (DSB):
*        Updated for 2nd internal release of IRA.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PRM_PAR'          ! STARLINK data constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRA_ERR'          ! IRA errors

*  Arguments Given:
      LOGICAL          FORWRD
      INTEGER          NVAL

*  Arguments Given and Returned:
      DOUBLE PRECISION C1( NVAL )
      DOUBLE PRECISION C2( NVAL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION A         ! Local longitude value.
      DOUBLE PRECISION ALPHA     ! Intermediate quantity, ALPHA.
      DOUBLE PRECISION B         ! Local latitude value.
      DOUBLE PRECISION BETA      ! Intermediate quantity, BETA.
      DOUBLE PRECISION COSB      ! COS of local latitude.
      DOUBLE PRECISION COSBET    ! COS of BETA.
      DOUBLE PRECISION COSR      ! COS of R.
      DOUBLE PRECISION H         ! Intermediate value
      DOUBLE PRECISION HALFA     ! Half of A
      INTEGER          I         ! Loop count.
      DOUBLE PRECISION R         ! Intermediate value
      DOUBLE PRECISION SINBET    ! SINE of BETA.
      DOUBLE PRECISION SINH      ! SINE of intermediate value H.
      DOUBLE PRECISION SINHA     ! SINE of half the local longitude.
      DOUBLE PRECISION SINR      ! SINE of intermediate value R.
      DOUBLE PRECISION SINRO2    ! SINE of half of intermediate value R.
      DOUBLE PRECISION SQRARG    ! Argument for SQRT function.
      DOUBLE PRECISION U         ! U value.
      DOUBLE PRECISION V         ! V value.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  First deal with forward transformations, from (U,V) coordinates to
*  local coordinates.
      IF( FORWRD ) THEN

*  Loop round all the input values.
         DO I = 1, NVAL
            U = C1(I)
            V = C2(I)

*  If either input is bad, set both outputs bad.
            IF( U .EQ. VAL__BADD .OR. V .EQ. VAL__BADD ) THEN
               C1(I) = VAL__BADD
               C2(I) = VAL__BADD

*  Otherwise, apply the transformation. See IRAS Catalogues and Atlasses
*  Explanatory Supplement, page X-32.
            ELSE

               SQRARG =  4.0 - 0.25*(U**2) - V**2
               IF( SQRARG .GE. 2.0D0 ) THEN

                  ALPHA = SQRT( SQRARG )
                  SINBET = 0.5*ALPHA*V
                  IF( ABS( SINBET ) .LE. 1.00001D0 ) THEN

                     BETA= ASIN( MAX( -1.0D0, MIN( 1.0D0, SINBET ) ) )
                     COSBET = COS( BETA )

                     IF( COSBET .NE. 0.0 ) THEN
                        SINHA = -0.25*ALPHA*U / COSBET
                        IF( ABS( SINHA ) .LE. 1.00001D0 ) THEN
                           C1(I) = 2.0*ASIN( MAX( -1.0D0,
     :                                           MIN( 1.0D0, SINHA ) ) )
                           C2(I) = BETA

                        ELSE
                           C1(I) = VAL__BADD
                           C2(I) = VAL__BADD

                        END IF

*  If the position is a pole, set the local longitude arbitrarily to
*  zero.
                     ELSE
                        C1(I) = 0.0
                        C2(I) = BETA

                     END IF

                  ELSE
                     C1(I) = VAL__BADD
                     C2(I) = VAL__BADD
                  END IF

               ELSE
                  C1(I) = VAL__BADD
                  C2(I) = VAL__BADD
               END IF

            END IF

         END DO

*  Now deal with inverse transformations, from local coordinates to
*  (U,V) coordinates.
      ELSE

*  Loop round all the input values.
         DO I = 1, NVAL
            A = C1(I)
            B = C2(I)

*  If either input is bad, set both outputs bad.
            IF( A .EQ. VAL__BADD .OR. B .EQ. VAL__BADD ) THEN
               C1(I) = VAL__BADD
               C2(I) = VAL__BADD

*  Otherwise...
            ELSE

*  Shift the latitude into the range +/- PI.
               B = MOD( B, 2.0D0*IRA__PI )
               IF( ABS( B ) .GE. IRA__PI ) B =
     :                                     B - SIGN( 2.0D0*IRA__PI, B )

*  If the absolute latitude is greater than PI/2, add PI onto the
*  longitude and subtract the latitude from PI.
               IF( ABS( B ) .GT. IRA__PIBY2 ) THEN
                  A = A + IRA__PI
                  B = SIGN( IRA__PI, B ) - B

               END IF

*  Shift the longitude into the range +/- PI.
               A = MOD( A, 2.0D0*IRA__PI )
               IF( ABS( A ) .GE. IRA__PI ) A =
     :                                     A - SIGN( 2.0D0*IRA__PI, A )

*  Apply the transformation. See IRAS Catalogues and Atlasses
*  Explanatory Supplement, page X-32.

               COSB = COS( B )
               HALFA = 0.5*A

               COSR = COSB*COS( HALFA )
               IF( ABS( COSR ) .LE. 1.0D0 ) THEN
                  R = ACOS( COSR )
                  SINR = SIN( R )

                  IF( SINR .NE. 0 ) THEN
                     SINH = COSB*SIN( HALFA )/SINR
                     IF( ABS( SINH ) .LE. 1.00001D0 ) THEN
                        H = ASIN( MAX( -1.0D0, MIN( 1.0D0, SINH ) ) )
                        SINRO2 = SIN( 0.5*R )

                        C1(I) = -4.0*SINRO2*SIN( H )
                        C2(I) = SIGN( 2.0D0, B )*SINRO2*COS( H )

                     ELSE
                        C1(I) = VAL__BADD
                        C2(I) = VAL__BADD
                     END IF

                  ELSE IF( COSR .GE. 1.0 ) THEN
                     C1(I) = 0.0D0
                     C2(I) = 0.0D0

                  ELSE
                     C1(I) = VAL__BADD
                     C2(I) = VAL__BADD
                  END IF

               ELSE
                  C1(I) = VAL__BADD
                  C2(I) = VAL__BADD
               END IF

            END IF

         END DO

      END IF

      END
