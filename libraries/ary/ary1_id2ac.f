      SUBROUTINE ARY1_ID2AC( IARY, IACB )
*+
*  Name:
*     ARY1_ID2AC

*  Purpose:
*     Convert an array identifier into the associated ACB index.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_ID2AC( IARY, IACB )

*  Description:
*     The routine converts an array identifier, previously issued by
*     ARY1_EXPID, into an index to the appropriate entry in the ACB.
*     The identifier supplied is fully checked and a value of zero is
*     returned if it is not valid.

*  Arguments:
*     IARY = INTEGER (Given)
*        Array identifier.
*     IACB = INTEGER (Returned)
*        Index to an entry in the ACB, or zero of the identifier
*        supplied was not valid.

*  Notes:
*     -  This routine does not perform error checking or reporting.

*  Algorithm:
*     -  Check that the IARY value supplied is positive and return a
*     value of zero if it is not.
*     -  Decode the IARY value into an index for the ACB.
*     -  Check that the identifier value matches the value originally
*     issued for the ACB slot and that the slot is still in use.
*     -  If everything is OK, then return the ACB index. Otherwise,
*     return zero.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     31-JUL-1989 (RFWS):
*        Original version.
*     22-AUG-1989 (RFWS):
*        Added EXTERNAL statement to ensure ARY1_INIT is linked.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants

*  Global Variables:
      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_CHK( ARY__MXACB ) = INTEGER (Read)
*           Identifier value originally issued for each ACB slot.
*        ACB_USED( ARY__MXACB ) = LOGICAL (Read)
*           Whether the ACB slot is in use.

*  Arguments Given:
      INTEGER IARY

*  Arguments Returned:
      INTEGER IACB

*  External References:
      EXTERNAL ARY1_INIT         ! Initialise common blocks

*  Local Variables:
      INTEGER I                  ! Temporary variable for ACB index

*.

*  Check that the identifier value is positive, and return a value of
*  zero if it is not.
      IF ( IARY .LE. 0 ) THEN
         IACB = 0

*  Decode the IARY value into an index for the ACB (the reverse of the
*  process used by ARY1_EXPID to encode it).
      ELSE
         I = MOD( IARY, ARY__MXACB )
         IF ( I .EQ. 0 ) I = ARY__MXACB

*  Check that the identifier matches the value originally issued for
*  this ACB slot and that the slot is still in use. If OK, then return
*  the ACB index. Otherwise, return zero.
         IF ( ( ACB_CHK( I ) .EQ. IARY ) .AND. ACB_USED( I ) ) THEN
            IACB = I
         ELSE
            IACB = 0
         END IF
      END IF

      END
