      SUBROUTINE NDF1_CHMOD( IACB, MODE, STATUS )
*+
*  Name:
*     NDF1_CHMOD

*  Purpose:
*     Check that the requested mode of mapped NDF access is permitted.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_CHMOD( IACB, MODE, STATUS )

*  Description:
*     The routine checks that the requested mode of mapped access is
*     permitted for an NDF. If it is not, then an error will be
*     reported. An error will also reported if the access mode string
*     supplied is not valid.  Otherwise the routine returns without
*     further action.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to The NDF entry in the ACB.
*     MODE = CHARACTER * ( * ) (Given)
*        The requested access mode; one of 'READ', 'WRITE' or 'UPDATE'
*        (case insensitive).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  For WRITE or UPDATE mode, call NDF1_CHACC to check whether
*     WRITE access to the NDF is permitted.
*     -  For READ access, no checks are necessary.
*     -  Report an error if the requested access mode string is
*     invalid.

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
*     3-OCT-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      INTEGER IACB
      CHARACTER * ( * ) MODE

*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If UPDATE or WRITE access is requested, then check that WRITE access
*  to the NDF is available.
      IF ( CHR_SIMLR( MODE, 'WRITE' ) .OR.
     :     CHR_SIMLR( MODE, 'UPDATE' ) ) THEN
         CALL NDF1_CHACC( IACB, 'WRITE', STATUS )

*  No action is needed if READ access is requested.
      ELSE IF ( CHR_SIMLR( MODE, 'READ' ) ) THEN
         CONTINUE

*  Report an error if the MODE value supplied is not recognised.
      ELSE
         STATUS = NDF__FATIN
         CALL MSG_SETC( 'ROUTINE', 'NDF1_CHMOD' )
         CALL MSG_SETC( 'BADMODE', MODE )
         CALL ERR_REP( 'NDF1_CHMOD_MODE',
     :   'Routine ^ROUTINE called with an invalid MODE argument ' //
     :   'of ''^BADMODE'' (internal programming error).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_CHMOD', STATUS )

      END
