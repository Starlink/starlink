      SUBROUTINE ARY1_BPPB( EL, ARRAY, BAD, STATUS )
*+
*  Name:
*     ARY1_BPPB

*  Purpose:
*     Determine if bad pixels are present in a vectorised BYTE array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_BPPB( EL, ARRAY, BAD, STATUS )

*  Description:
*     The routine examines the values in a vectorised BYTE array and
*     returns a logical result BAD indicating whether any element of the
*     array contains the "bad" pixel value VAL__BADB.

*  Arguments:
*     EL = INTEGER (Given)
*        Number of elements in the vectorised array.
*     ARRAY( EL ) = BYTE (Given)
*        The array to be examined.
*     BAD = LOGICAL (Returned)
*        Whether any ARRAY element had the value VAL__BADB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Initialise.
*     -  Loop to examine each array element.
*     -  If a bad value is found, then set BAD=.TRUE. and quit checking.

*  Copyright:
*     Copyright (C) 1989 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-NOV-1989 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants

*  Arguments Given:
      INTEGER EL
      BYTE ARRAY( EL )

*  Arguments Returned:
      LOGICAL BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter for array elements

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      BAD = .FALSE.

*  Loop to examine each array element.
      DO 1 I = 1, EL

*  If a bad value is found, set BAD=.TRUE. and quit checking.
         IF ( ARRAY( I ) .EQ. VAL__BADB ) THEN
            BAD = .TRUE.
            GO TO 2
         END IF
1     CONTINUE
2     CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_BPPB',
     :                                            STATUS )

      END
