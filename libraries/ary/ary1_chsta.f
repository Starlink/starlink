      SUBROUTINE ARY1_CHSTA( IACB, MODE, STATUS )
*+
*  Name:
*     ARY1_CHSTA

*  Purpose:
*     Check if the current state of an array permits the requested
*     mode of access.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_CHSTA( IACB, MODE, STATUS )

*  Description:
*     The routine checks if the current state of an array (i.e. defined
*     or undefined) permits the requested mode of access (data may not
*     be read from the array if it is undefined, so READ and UPDATE
*     access are not permitted in these circumstances). An error is
*     reported if the requested access is forbidden, otherwise the
*     routine returns without action.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index of entry in ACB.
*     MODE = CHARACTER * ( * ) (Given)
*        The access mode requested. The values 'READ', 'WRITE' or
*        'UPDATE' may be specified (case insensitive).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Test the requested access mode string against the permitted
*     values.
*     -  If WRITE access is requested, then no checks are needed.
*     -  If READ or UPDATE access is requested, then obtain the index
*     for the data object entry in the DCB and ensure that state
*     information is available for it.
*     -  If the array's data values are undefined, then report an error.
*     -  If the requested access mode string was not valid, then report
*     an error.

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
*     13-JUN-1989 (RFWS):
*        Original version.
*     1-SEP-1989 (RFWS):
*        Changed to use message token to prevent '$' affecting error
*        message.
*     18-SEP-1989 (RFWS):
*        Made minor improvement to error message.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
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
*           Locator to data object.
*        DCB_STA( ARY__MXDCB ) = LOGICAL (Read)
*           Whether the array's data values are defined.

      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_IDCB( ARY__MXACB ) = INTEGER (Read)
*           Index to the data object entry in the DCB.

*  Arguments Given:
      INTEGER IACB
      CHARACTER * ( * ) MODE

*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*  Local variables:
      INTEGER IDCB               ! Data object index in DCB
      CHARACTER * ( ARY__SZMOD ) UMODE ! Upper case MODE value

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If WRITE access is requested, then no checks are needed.
      IF ( CHR_SIMLR( MODE, 'WRITE' ) ) THEN
         CONTINUE

*  If READ or UPDATE access is requested, then obtain the DCB index for
*  the data object and ensure that state information is available for
*  it.
      ELSE IF ( CHR_SIMLR( MODE, 'READ' ) .OR.
     :          CHR_SIMLR( MODE, 'UPDATE' ) ) THEN
         IDCB = ACB_IDCB ( IACB )
         CALL ARY1_DSTA( IDCB, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 9999

*  If the array's data values are undefined, then report an error.
         IF ( .NOT. DCB_STA( IDCB ) ) THEN
            STATUS = ARY__UNDEF
            CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
            UMODE = MODE
            CALL CHR_UCASE( UMODE )
            CALL MSG_SETC( 'BADMODE', UMODE )
            CALL ERR_REP( 'ARY1_CHSTA_BAD',
     :      '^BADMODE access to the array ^ARRAY is not available; ' //
     :      'the array is in an undefined state (possible ' //
     :      'programming error).', STATUS )
         END IF

*  If the requested access mode string was not valid, then report an
*  error.
      ELSE
         STATUS = ARY__FATIN
         CALL MSG_SETC( 'ROUTINE', 'ARY1_CHSTA' )
         CALL MSG_SETC( 'BADMODE', MODE )
         CALL ERR_REP( 'ARY1_CHSTA_MODE',
     :   'Routine ^ROUTINE called with an invalid MODE argument ' //
     :   'of ''^BADMODE'' (internal programming error).', STATUS )
      END IF
9999  CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_CHSTA', STATUS )

      END
