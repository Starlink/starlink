      SUBROUTINE NDF1_HTCMP( YMDHM1, SEC1, YMDHM2, SEC2, ORDER, STATUS )
*+
*  Name:
*     NDF1_HTCMP

*  Purpose:
*     Compare two history times to determine their order.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_HTCMP( YMDHM1, SEC1, YMDHM2, SEC2, ORDER, STATUS )

*  Description:
*     The routine compares two times and returns an indication of which
*     is earlier or later.

*  Arguments:
*     YMDHM1( 5 ) = INTEGER (Given)
*        The years, months, days, hours and minutes fields of the first
*        time, stored in that order.
*     SEC1 = REAL (Given)
*        The seconds field of the first time.
*     YMDHM2( 5 ) = INTEGER (Given)
*        The years, months, days, hours and minutes fields of the
*        second time, stored in that order.
*     SEC2 = REAL (Given)
*        The seconds field of the second time.
*     ORDER = INTEGER (Returned)
*        Returns +1 if the second time is later than the first one, -1
*        if the reverse is true, and 0 if both times are the same.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     19-MAY-1993 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER YMDHM1( 5 )
      REAL SEC1
      INTEGER YMDHM2( 5 )
      REAL SEC2

*  Arguments Returned:
      INTEGER ORDER

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter for YMDHM fields

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      ORDER = 0

*  Inspect each field in the YMDHM arrays starting at the most
*  significant (years) end.
      DO 1 I = 1, 5

*  Detect if the first date/time is larger.
         IF ( YMDHM1( I ) .GT. YMDHM2( I ) ) THEN
            ORDER = -1
            GO TO 2

*  Detect if the second date/time is larger.
         ELSE IF ( YMDHM1( I ) .LT. YMDHM2( I ) ) THEN
            ORDER = 1
            GO TO 2
         END IF
 1    CONTINUE
 2    CONTINUE

*  If all the YMDHM fields are the same, then compare the seconds
*  fields in the same way.
      IF ( ORDER .EQ. 0 ) THEN
         IF ( SEC1 .GT. SEC2 ) THEN
            ORDER = -1
         ELSE IF ( SEC1 .LT. SEC2 ) THEN
            ORDER = 1
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_HTCMP', STATUS )

      END
