      SUBROUTINE CCD1_DXY12( STRING, VALUES, STATUS )
*+
*  Name:
*     CCD1_DXY12

*  Purpose:
*     Decode a '[X1:X2,Y1:Y2]' format string.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_DXY12( STRING, INDXY, VALUES, STATUS )

*  Description:
*     This routine parses a string of the form "[X1:X2,Y1:Y2]" and returns
*     a 4-element double precision vector.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The string containing the value, of the form "[X1:X2,Y1:Y2]",
*        where X1, X2, Y1 and Y2 can be read as numerical values.
*     VALUES( 4 ) = DOUBLE PRECISION (Returned)
*        A vector containing X1, X2, Y1, Y2.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils

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
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-AUG-2000 (MBT):
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
      CHARACTER STRING * ( * )

*  Arguments Returned:
      DOUBLE PRECISION VALUES( 4 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable
      INTEGER IAT                ! Current position in string
      INTEGER START( 4 )         ! Character position of start of each number
      INTEGER STOP( 4 )          ! Character position of end of each number
      LOGICAL OK                 ! Have we found all expected delimiters?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise substring start and end markers.
      DO I = 1, 4
         START( I ) = 0
         STOP( I ) = 0
      END DO

*  Work through the string marking start and end of each number.
      IAT = 1
      CALL CHR_TOCHR( '[', STRING, .TRUE., IAT )
      START( 1 ) = IAT + 1
      CALL CHR_TOCHR( ':', STRING, .TRUE., IAT )
      STOP( 1 ) = IAT - 1
      START( 2 ) = IAT + 1
      CALL CHR_TOCHR( ',', STRING, .TRUE., IAT )
      STOP( 2 ) = IAT - 1
      START( 3 ) = IAT + 1
      CALL CHR_TOCHR( ':', STRING, .TRUE., IAT )
      STOP( 3 ) = IAT - 1
      START( 4 ) = IAT + 1
      CALL CHR_TOCHR( ']', STRING, .TRUE., IAT )
      STOP( 4 ) = IAT - 1

*  Check that we found all the delimiters we were looking for.
      OK = START( 1 ) .LE. STOP( 1 ) .AND.
     :     START( 2 ) .LE. STOP( 2 ) .AND.
     :     START( 3 ) .LE. STOP( 3 ) .AND.
     :     START( 4 ) .LE. STOP( 4 ) .AND.
     :     START( 1 ) .LT. START( 2 ) .AND.
     :     START( 2 ) .LT. START( 3 ) .AND.
     :     START( 3 ) .LT. START( 4 )

*  Do conversion of the substrings to numbers.
      IF ( OK ) THEN
         DO I = 1, 4
            CALL CHR_CTOD( STRING( START( I ) : STOP( I ) ),
     :                     VALUES( I ), STATUS )
         END DO
      END IF

      END
* $Id$
