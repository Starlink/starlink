      SUBROUTINE IRA1_VGNOM( FORWRD, NVAL, C1, C2, OK, STATUS )
*+
*  Name:
*     IRA1_VGNOM

*  Purpose:
*     Check for valid coordinates using a Gnomonic projection.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_VGNOM( FORWRD, NVAL, C1, C2, OK, STATUS )

*  Description:
*     Returns flags indicating if valid coordinates would result if a
*     forward or inverse GNOMONIC projection was applied to the given
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
*        The number of coordinate points to be checked.
*     C1( NVAL ) = DOUBLE PRECISION (Given)
*        The input U or longitude values.
*     C2( NVAL ) = DOUBLE PRECISION (Given)
*        The input V or latitude values.
*     OK( NVAL ) = LOGICAL (Returned)
*        True if the corresponding input coordinates would transform to
*        valid output coordinates.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-FEB-1992 (DSB):
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
      INTEGER          I         ! Loop count.
      DOUBLE PRECISION SINB      ! SIN of local latitude.
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

*  Otherwise, apply the transformation. See IRAS Catalogues and Atlases
*  Explanatory Supplement, Page X-30 ( the equivalents of alpha_0 and
*  delta_0 are set to zero here).
            ELSE

               SINB =  V/SQRT( U**2 + V**2 + 1.0 )
               IF( ABS( SINB ) .LE. 1.00001D0 ) THEN
                  OK(I) = .TRUE.
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
               OK(I) = .FALSE.

*  Otherwise, apply the transformation.
            ELSE
               COSA = COS( A )
               IF( COSA .GT. 0.0D0 ) THEN
                  OK(I) = .TRUE.

               ELSE
                  OK(I) = .FALSE.

               END IF

            END IF

         END DO

      END IF

      END
