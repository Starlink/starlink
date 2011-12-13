      SUBROUTINE IRA1_GNOM( FORWRD, NVAL, C1, C2, STATUS )
*+
*  Name:
*     IRA1_GNOM

*  Purpose:
*     Transform coordinate data using a Gnomonic projection.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_GNOM( FORWRD, NVAL, C1, C2, STATUS )

*  Description:
*     Applies a forward or inverse GNOMONIC projection to the supplied
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
*     17-APR-1991 (DSB):
*        Modified for second version of IRA.
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

*  Arguments Given and Returned:
      DOUBLE PRECISION C1( NVAL )
      DOUBLE PRECISION C2( NVAL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION A         ! Local longitude value.
      DOUBLE PRECISION B         ! Local latitude value.
      DOUBLE PRECISION COSA      ! COS of local longitude.
      INTEGER          I         ! Loop count.
      DOUBLE PRECISION SINB      ! SIN of local latitude.
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

*  If either input is bad, set both outputs bad.
            IF( U .EQ. VAL__BADD .OR. V .EQ. VAL__BADD ) THEN
               C1(I) = VAL__BADD
               C2(I) = VAL__BADD

*  Otherwise, apply the transformation. See IRAS Catalogues and Atlases
*  Explanatory Supplement, Page X-30 ( the equivalents of alpha_0 and
*  delta_0 are set to zero here).
            ELSE

               SINB =  V/SQRT( U**2 + V**2 + 1.0 )
               IF( ABS( SINB ) .LE. 1.00001D0 ) THEN
                  C1(I) = ATAN( -U )
                  C2(I) = ASIN( MAX( -1.0D0, MIN( 1.0D0, SINB ) ) )

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

*  Otherwise, apply the transformation.
            ELSE
               COSA = COS( A )
               IF( COSA .GT. 0.0D0 ) THEN
                  C1(I) = -TAN( A )
                  C2(I) = TAN( B )/COSA

               ELSE
                  C1(I) = VAL__BADD
                  C2(I) = VAL__BADD
               END IF

            END IF

         END DO

      END IF

      END
