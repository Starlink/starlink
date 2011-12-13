      SUBROUTINE NDF1_CHACC( IACB, ACCESS, STATUS )
*+
*  Name:
*     NDF1_CHACC

*  Purpose:
*     Check that a specified type of access to an ACB entry is
*     permitted.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_CHACC( IACB, ACCESS, STATUS )

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
*     26-SEP-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      INTEGER IACB
      CHARACTER * ( * ) ACCESS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( NDF__SZACC ) UACC ! Upper case version of ACCESS
      LOGICAL OK                 ! Whether requested access is permitted

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine if the required type of access is available.
      CALL NDF1_ACCOK( IACB, ACCESS, OK, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  If it is not, then report an error.
         IF ( .NOT. OK ) THEN
            STATUS = NDF__ACDEN
            CALL NDF1_AMSG( 'NDF', IACB )

*  ...Use an upper case version of the access type.
            UACC = ACCESS
            CALL CHR_UCASE( UACC )
            CALL MSG_SETC( 'ACCESS', UACC )
            CALL ERR_REP( 'NDF1_CHACC_NO',
     :      '^ACCESS access to the NDF structure ^NDF is not ' //
     :      'available via the specified identifier or has been ' //
     :      'disabled (possible programming error).', STATUS )
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_CHACC', STATUS )

      END
