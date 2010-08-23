      SUBROUTINE IRA_NORM( A, B, STATUS )
*+
*  Name:
*     IRA_NORM

*  Purpose:
*     Converts sky co-ordinate values to the equivalent first-order
*     values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_NORM( A, B, STATUS )

*  Description:
*     The given latitude value is shifted into the range +/- PI/2 ( a
*     shift of PI may be introduced in the longitude value to achieve
*     this). The longitude value is then shifted into the range 0 to
*     2*PI. If either A or B has the Starlink "BAD" value on entry
*     (VAL__BADD) then both A and B are left unchanged on exit. Latitude
*     values which are within 0.01 arcseconds of either pole are
*     modified to put them exactly at the pole.

*  Arguments:
*     A = DOUBLE PRECISION (Given and Returned)
*        The longitude, in radians. On exit, the value is shifted in to
*        the range 0 to 2*PI.
*     B = DOUBLE PRECISION (Given and Returned)
*        The latitude value, in radians. On exit, the value is shifted
*        in to the range -PI/2 to +PI/2.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991, 1992 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-JAN-1991 (DSB):
*        Original version.
*     27-APR-1991 (DSB):
*        Modified for IRA version 2.
*     4-MAR-1992 (DSB):
*        Latitude values within 0.01 arcseconds of either pole are
*        treated as if at the pole.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.

*  Arguments Given and Returned:
      DOUBLE PRECISION A
      DOUBLE PRECISION B

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      DOUBLE PRECISION DANGLE    ! 0.01 arc-seconds in radians.
      PARAMETER ( DANGLE = 4.8482D-8 )

*  Local Variables:
      DOUBLE PRECISION NEWA      ! Buffer for output A value.
      DOUBLE PRECISION NEWB      ! Buffer for output B value.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check for bad input values.
      IF( A .NE. VAL__BADD .AND. B .NE. VAL__BADD ) THEN

*  Ensure the latitude is in the range +/- pi.
         NEWB = MOD( B, 2.0D0*IRA__PI )
         IF( ABS( NEWB ) .GE. IRA__PI ) NEWB  = NEWB -
     :                                  SIGN( 2.0D0*IRA__PI, B )

*  If the latitude is now outside the range +/- (pi/2 rads + 0.01
*  arc-seconds), then shift it into that range, and change the
*  longitude by pi.
         IF( ABS( NEWB ) .GE. IRA__PIBY2 + DANGLE ) THEN
            NEWB = SIGN( IRA__PI, NEWB ) - NEWB
            A = A + IRA__PI
         END IF

*  If the latitude value is in the "tolerance range" (90 degs to
*  90 degs + 0.01 arc-seconds), limit the latitude to 90 degs.
         IF( NEWB .GT. IRA__PIBY2 ) THEN
            NEWB = IRA__PIBY2

         ELSE IF( NEWB .LT. -IRA__PIBY2 ) THEN
            NEWB = -IRA__PIBY2

         END IF

*  Now ensure that the longitude is in the range 0 - 2*pi.
         NEWA = MOD( A, 2.0D0*IRA__PI )
         IF( NEWA .LT. 0.0D0 ) NEWA = NEWA + 2.0D0*IRA__PI

*  Return the new values.
         A = NEWA
         B = NEWB

*  Change longitude=(2*PI radians +/- 0.01 arcseconds) to longitiude=0
         IF( ABS( 2.0*IRA__PI - A) .LT. DANGLE ) A = 0.0

      END IF

      END
