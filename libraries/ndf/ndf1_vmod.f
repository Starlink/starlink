      SUBROUTINE NDF1_VMOD( MODE, VMODE, STATUS )
*+
*  Name:
*     NDF1_VMOD

*  Purpose:
*     Validate an access mode string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_VMOD( MODE, VMODE, STATUS )

*  Description:
*     The routine validates an access mode string, returning an upper
*     case version if it is valid. Otherwise, an error is reported.

*  Arguments:
*     MODE = CHARACTER * ( * ) (Given)
*        The access mode string to be validated. Valid values are
*        'READ', 'UPDATE' or 'WRITE' (case insensitive).
*     VMODE = CHARACTER * ( * ) (Returned)
*        The validated access mode string in upper case (not returned if
*        the MODE value is invalid).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Check the access mode value supplied against each permitted
*     value in turn, returning the appropriate validated value.
*     -  If the access mode was not recognised, then report an error.

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
*     6-OCT-1989 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      CHARACTER * ( * ) MODE

*  Arguments Returned:
      CHARACTER * ( * ) VMODE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the access mode string supplied against each permitted value in
*  turn, returning the appropriate validated value.

*  ...READ access.
      IF ( CHR_SIMLR( MODE, 'READ' ) ) THEN
         CALL NDF1_CCPY( 'READ', VMODE, STATUS )

*  ...UPDATE access.
      ELSE IF ( CHR_SIMLR( MODE, 'UPDATE' ) ) THEN
         CALL NDF1_CCPY( 'UPDATE', VMODE, STATUS )

*  ...WRITE access.
      ELSE IF ( CHR_SIMLR( MODE, 'WRITE' ) ) THEN
         CALL NDF1_CCPY( 'WRITE', VMODE, STATUS )

*  If the access mode value was not recognised, then report an error.
      ELSE
         STATUS = NDF__MODIN
         CALL MSG_SETC( 'BADMODE', MODE )
         CALL ERR_REP( 'NDF1_VMOD_BAD',
     :   'Invalid access mode ''^BADMODE'' specified (possible ' //
     :   'programming error).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_VMOD', STATUS )

      END
