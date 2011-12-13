      SUBROUTINE ARY1_CHACC( IACB, ACCESS, STATUS )
*+
*  Name:
*     ARY1_CHACC

*  Purpose:
*     Check that a specified type of access to an ACB entry is
*     permitted.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_CHACC( IACB, ACCESS, STATUS )

*  Description:
*     The routine checks that the specified type of access to an ACB
*     entry is permitted. If it is, then it returns without further
*     action, otherwise an error is reported.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to the ACB entry.
*     ACCESS = CHARACTER * ( * ) (Given)
*        The type of access required (case insensitive).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Determine whether the requested type of access is permitted.
*     -  If it is not, then report an error.

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
*     13-SEP-1989 (RFWS):
*        Original version.
*     13-SEP-1989 (RFWS):
*        Fixed bug in assigning the array name to a message token.
*     18-SEP-1989 (RFWS):
*        Improved the error message.
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
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_LOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Data object locator.

      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_IDCB( ARY__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER IACB
      CHARACTER * ( * ) ACCESS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( ARY__SZACC ) UACC ! Upper case version of ACCESS
      INTEGER IDCB               ! Index to data object entry in the DCB
      LOGICAL OK                 ! Whether requested access is permitted

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine if the required type of access is available.
      CALL ARY1_ACCOK( IACB, ACCESS, OK, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  If it is not, then report an error.
         IF ( .NOT. OK ) THEN
            STATUS = ARY__ACDEN
            IDCB = ACB_IDCB( IACB )
            CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )

*  ...Use an upper case version of the access type.
            UACC = ACCESS
            CALL CHR_UCASE( UACC )
            CALL MSG_SETC( 'ACCESS', UACC )
            CALL ERR_REP( 'ARY1_CHACC_NO', '^ACCESS access to the '//
     :                    'array ^ARRAY is not available or has been '//
     :                    'disabled (possible programming error).',
     :                    STATUS )
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_CHACC', STATUS )

      END
