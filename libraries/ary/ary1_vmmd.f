      SUBROUTINE ARY1_VMMD( MMOD, MODE, INOPT, STATUS )
*+
*  Name:
*     ARY1_VMMD

*  Purpose:
*     Validate a mapping mode specification.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_VMMD( MMOD, MODE, INOPT, STATUS )

*  Description:
*     The routine checks a string containing a mapping mode
*     specification for validity. If it is valid, the mapping access
*     mode and the initialisation option string for write access are
*     returned. If the mapping mode specification is not valid, then an
*     error is reported.

*  Arguments:
*     MMOD = CHARACTER * ( * ) (Given)
*        The mapping mode string to be validated. Valid values are
*        'READ', 'UPDATE' or 'WRITE', with an initialisation option
*        '/ZERO' or '/BAD' optionally appended (case insensitive).
*     MODE = CHARACTER * ( * ) (Returned)
*        The mapping access mode (either 'READ', 'UPDATE' or 'WRITE',
*        in upper case).
*     INOPT = CHARACTER * ( * ) (Returned)
*        The initialisation option (either 'ZERO', 'BAD' or ' ') in
*        upper case.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Compare the mapping mode string with each of the permitted
*     values in turn, setting the returned arguments accordingly.
*     -  If the mapping mode string is not valid, then report an error.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
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
*     9-JUN-1989  (RFWS):
*        Original version.
*     8-AUG-1989 (RFWS):
*        Fixed bug in calling sequence of ARY1_CCPY.
*     16-JAN-1990 (RFWS):
*        Changed to allow initialisation options on all access modes.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Arguments Given:
      CHARACTER * ( * ) MMOD

*  Arguments Returned:
      CHARACTER * ( * ) MODE
      CHARACTER * ( * ) INOPT

*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Test the mapping mode string against each permitted value in turn,
*  setting the returned arguments accordingly.

*  ...Read access.
      IF ( CHR_SIMLR( MMOD, 'READ' ) ) THEN
         CALL ARY1_CCPY( 'READ', MODE, STATUS )
         INOPT = ' '

      ELSE IF ( CHR_SIMLR( MMOD, 'READ/ZERO' ) ) THEN
         CALL ARY1_CCPY( 'READ', MODE, STATUS )
         CALL ARY1_CCPY( 'ZERO', INOPT, STATUS )

      ELSE IF ( CHR_SIMLR( MMOD, 'READ/BAD' ) ) THEN
         CALL ARY1_CCPY( 'READ', MODE, STATUS )
         CALL ARY1_CCPY( 'BAD', INOPT, STATUS )

*  ...Update access.
      ELSE IF ( CHR_SIMLR( MMOD, 'UPDATE' ) ) THEN
         CALL ARY1_CCPY( 'UPDATE', MODE, STATUS )
         INOPT = ' '

      ELSE IF ( CHR_SIMLR( MMOD, 'UPDATE/ZERO' ) ) THEN
         CALL ARY1_CCPY( 'UPDATE', MODE, STATUS )
         CALL ARY1_CCPY( 'ZERO', INOPT, STATUS )

      ELSE IF ( CHR_SIMLR( MMOD, 'UPDATE/BAD' ) ) THEN
         CALL ARY1_CCPY( 'UPDATE', MODE, STATUS )
         CALL ARY1_CCPY( 'BAD', INOPT, STATUS )

*  ...Write access.
      ELSE IF ( CHR_SIMLR( MMOD, 'WRITE' ) ) THEN
         CALL ARY1_CCPY( 'WRITE', MODE, STATUS )
         INOPT = ' '

      ELSE IF ( CHR_SIMLR( MMOD, 'WRITE/ZERO' ) ) THEN
         CALL ARY1_CCPY( 'WRITE', MODE, STATUS )
         CALL ARY1_CCPY( 'ZERO', INOPT, STATUS )

      ELSE IF ( CHR_SIMLR( MMOD, 'WRITE/BAD' ) ) THEN
         CALL ARY1_CCPY( 'WRITE', MODE, STATUS )
         CALL ARY1_CCPY( 'BAD', INOPT, STATUS )

*  If the mapping mode string was not valid, then report an error.
      ELSE
         STATUS = ARY__MMDIN
         CALL MSG_SETC( 'BADMMODE', MMOD )
         CALL ERR_REP( 'ARY1_VMMD_BAD',
     :   'Invalid array mapping mode ''^BADMMODE'' specified ' //
     :   '(possible programming error).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_VMMD', STATUS )

      END
