      SUBROUTINE IRA1_VORTH( FORWRD, NVAL, C1, C2, OK, STATUS )
*+
*  Name:
*     IRA1_VORTH

*  Purpose:
*     Check for valid coordinates using an Orthographic projection.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_VORTH( FORWRD, NVAL, C1, C2, OK, STATUS )

*  Description:
*     Returns flags indicating if valid coordinates would result if a
*     forward or inverse ORTHOGRAPHIC projection was applied to the
*     given coordinate data. The coordinates themselves are not
*     calculated.  If FORWRD is true (image to sky conversion), C1 and
*     C2 should contain values of U and V on entry. If FORWRD is false,
*     the opposite is true. See routine IRA1_IPRJ for a description of
*     local coordinates and (U,V) coordinates.

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
*        Orginal version.
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
      LOGICAL FORWRD
      INTEGER NVAL
      DOUBLE PRECISION C1( NVAL )
      DOUBLE PRECISION C2( NVAL )

*  Arguments Returned:
      LOGICAL OK( NVAL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION A         ! Local longitude value.
      DOUBLE PRECISION B         ! Local latitude value.
      DOUBLE PRECISION COSA      ! COS of local longitude.
      DOUBLE PRECISION COSB      ! COS of local latitude.
      INTEGER          I         ! Loop count.
      DOUBLE PRECISION SQRARG    ! Argument of SQRT function.
      DOUBLE PRECISION U         ! U coordinate value.
      DOUBLE PRECISION V         ! V coordinate value.

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

*  Otherwise, evaluate the argument for the square root function used
*  in the transformation.
            ELSE
               SQRARG = 1 - V**2

*  Return bad A and B values if illegal U and V values were supplied.
               IF( SQRARG .LT. U**2 ) THEN
                  OK(I) = .FALSE.

*  Otherwise, apply the transformation.
               ELSE IF( SQRARG .GT. 0.0D0 ) THEN
                  OK(I) = .TRUE.

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
               OK(I) = .FALSE.

*  Otherwise, apply the transformation.
            ELSE
               COSA = COS( A )
               COSB = COS( B )

               IF( COSB*COSA .GE. 0.0D0 ) THEN
                  OK(I) = .TRUE.

               ELSE
                  OK(I) = .FALSE.

               END IF

            END IF

         END DO

      END IF

      END
