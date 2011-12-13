      SUBROUTINE NDF1_VMMD( MMOD, MODE, INOPT, STATUS )
*+
*  Name:
*     NDF1_VMMD

*  Purpose:
*     Validate a mapping mode specification.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_VMMD( MMOD, MODE, INOPT, STATUS )

*  Description:
*     The routine checks a string containing a mapping mode
*     specification for validity. If it is valid, the mapping access
*     mode and the initialisation option string for write access are
*     returned. If the mapping mode specification is not valid, then an
*     error is reported.

*  Arguments:
*     MMOD = CHARACTER * ( * ) (Given)
*        The mapping mode string to be validated. Valid values are
*        'READ', 'UPDATE' or 'WRITE' with either of the initialisation
*        options '/ZERO' or '/BAD' optionally appended (case
*        insensitive).
*     MODE = CHARACTER * ( * ) (Returned)
*        The mapping access mode (either 'READ', 'WRITE' or 'UPDATE')
*        in upper case.
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
*     20-DEC-1989 (RFWS):
*        Changed to allow an initialisation option to be appended to any
*        access mode.
*     {enter_further_changes_here}

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
         CALL NDF1_CCPY( 'READ', MODE, STATUS )
         INOPT = ' '

      ELSE IF ( CHR_SIMLR( MMOD, 'READ/ZERO' ) ) THEN
         CALL NDF1_CCPY( 'READ', MODE, STATUS )
         CALL NDF1_CCPY( 'ZERO', INOPT, STATUS )

      ELSE IF ( CHR_SIMLR( MMOD, 'READ/BAD' ) ) THEN
         CALL NDF1_CCPY( 'READ', MODE, STATUS )
         CALL NDF1_CCPY( 'BAD', INOPT, STATUS )

*  ...Write access.
      ELSE IF ( CHR_SIMLR( MMOD, 'WRITE' ) ) THEN
         CALL NDF1_CCPY( 'WRITE', MODE, STATUS )
         INOPT = ' '

      ELSE IF ( CHR_SIMLR( MMOD, 'WRITE/ZERO' ) ) THEN
         CALL NDF1_CCPY( 'WRITE', MODE, STATUS )
         CALL NDF1_CCPY( 'ZERO', INOPT, STATUS )

      ELSE IF ( CHR_SIMLR( MMOD, 'WRITE/BAD' ) ) THEN
         CALL NDF1_CCPY( 'WRITE', MODE, STATUS )
         CALL NDF1_CCPY( 'BAD', INOPT, STATUS )

*  ...Update access.
      ELSE IF ( CHR_SIMLR( MMOD, 'UPDATE' ) ) THEN
         CALL NDF1_CCPY( 'UPDATE', MODE, STATUS )
         INOPT = ' '

      ELSE IF ( CHR_SIMLR( MMOD, 'UPDATE/ZERO' ) ) THEN
         CALL NDF1_CCPY( 'UPDATE', MODE, STATUS )
         CALL NDF1_CCPY( 'ZERO', INOPT, STATUS )

      ELSE IF ( CHR_SIMLR( MMOD, 'UPDATE/BAD' ) ) THEN
         CALL NDF1_CCPY( 'UPDATE', MODE, STATUS )
         CALL NDF1_CCPY( 'BAD', INOPT, STATUS )

*  If the mapping mode string was not valid, then report an error.
      ELSE
         STATUS = NDF__MMDIN
         CALL MSG_SETC( 'BADMMODE', MMOD )
         CALL ERR_REP( 'NDF1_VMMD_BAD',
     :   'Invalid mapping mode ''^BADMMODE'' specified (possible ' //
     :   'programming error).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_VMMD', STATUS )

      END
