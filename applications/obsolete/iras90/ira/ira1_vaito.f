      SUBROUTINE IRA1_VAITO( FORWRD, NVAL, C1, C2, OK, STATUS )
*+
*  Name:
*     IRA1_VAITO

*  Purpose:
*     Check for valid coordinates using an Aitoff projection.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_VAITO( FORWRD, NVAL, C1, C2, OK, STATUS )

*  Description:
*     Returns flags indicating if valid coordinates would result if a
*     forward or inverse AITOFF projection was applied to the given
*     coordinate data. The coordinates themselves are not calculated.
*     If FORWRD is true (image to sky conversion), C1 and C2 should
*     contain values of U and V on entry. If FORWRD is false, the
*     opposite is true. See routine IRA1_IPRJ for a description of
*     local coordinates and (U,V) coordinates.

*  Arguments:
*     FORWRD = LOGICAL (Given)
*        If true then the forward mapping is used from (U,V) coordinates
*        to local coordinates. Otherwise, the inverse mapping from local
*        coordinates to (U,V) coordinates is used.
*     NVAL = INTEGER (Given)
*        The number of coordinate points to be transformed.
*     C1( NVAL ) = DOUBLE PRECISION (Given and Returned)
*        Input longitude or U values.
*     C2( NVAL ) = DOUBLE PRECISION (Given)
*        Input latitude or V values.
*     OK( NVAL ) = LOGICAL (Returned)
*        True if the corresponding input coordinates would transform to
*        valid output coordinates.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-FEB-1992 (DSB):
*        Original version.
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
      DOUBLE PRECISION C1( NVAL )
      DOUBLE PRECISION C2( NVAL )

*  Arguments Returned:
      LOGICAL OK( NVAL )

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
      DOUBLE PRECISION HALFA     ! Half of A
      INTEGER          I         ! Loop count.
      DOUBLE PRECISION R         ! Intermediate value
      DOUBLE PRECISION SINBET    ! SINE of BETA.
      DOUBLE PRECISION SINH      ! SINE of intermediate value H.
      DOUBLE PRECISION SINHA     ! SINE of half the local longitude.
      DOUBLE PRECISION SINR      ! SINE of intermediate value R.
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

*  If either input is bad, set output bad.
            IF( U .EQ. VAL__BADD .OR. V .EQ. VAL__BADD ) THEN
               OK(I) = .FALSE.

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
                           OK(I) = .TRUE.

                        ELSE
                           OK(I) = .FALSE.

                        END IF

*  If the position is a pole, set the local longitude arbitrarily to
*  zero.
                     ELSE
                        OK(I ) = .TRUE.

                     END IF

                  ELSE
                     OK(I) = .FALSE.
                  END IF

               ELSE
                  OK(I) = .FALSE.
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

*  If either input is bad, set output bad.
            IF( A .EQ. VAL__BADD .OR. B .EQ. VAL__BADD ) THEN
               OK(I) = .TRUE.

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
                        OK(I) = .TRUE.

                     ELSE
                        OK(I) = .FALSE.

                     END IF

                  ELSE IF( COSR .GE. 1.0 ) THEN
                     OK(I) = .TRUE.

                  ELSE
                     OK(I) = .FALSE.
                  END IF

               ELSE
                  OK(I) = .FALSE.
               END IF

            END IF

         END DO

      END IF

      END
