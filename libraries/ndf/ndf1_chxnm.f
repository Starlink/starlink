      SUBROUTINE NDF1_CHXNM( XNAME, STATUS )
*+
*  Name:
*     NDF1_CHXNM

*  Purpose:
*     Check an NDF extension name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_CHXNM( XNAME, STATUS )

*  Description:
*     The routine checks the name of an NDF extension for standard
*     form. A standard name must be no more than NDF__SZXNM characters
*     long, must begin with an alphabetic character and continue with
*     alphanumeric characters (including underscore) only. If this test
*     fails, then an error is reported and a STATUS value set.
*     Otherwise, the routine returns without action.

*  Arguments:
*     XNAME = CHARACTER * ( * ) (Given)
*        The extension name to be checked.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Check that the extension name is not too long and that it has
*     the standard form. Report an error if appropriate.

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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-SEP-1989 (RFWS):
*        Original version.
*     26-SEP-1989 (RFWS):
*        Finished prologue.
*     23-NOV-1989 (RFWS):
*        Changed to use the NDF__SZXNM constant.
*     29-JAN-1990 (RFWS):
*        Removed checks on name registration.
*     {enter_further_changes_here}

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
      CHARACTER * ( * ) XNAME

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_ISNAM          ! Whether a string is a standard name
      INTEGER CHR_LEN            ! Significant length of string

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the extension name is too long, or does not have the correct
*  standard form, then report an error.
      IF ( ( CHR_LEN( XNAME ) .GT. NDF__SZXNM ) .OR.
     :     ( .NOT. CHR_ISNAM( XNAME ) ) ) THEN
         STATUS = NDF__NSXNM
         CALL MSG_SETC( 'XNAME', XNAME )
         CALL ERR_REP( 'NDF1_CHXNM_NS',
     :   'Non-standard extension name ''^XNAME'' specified ' //
     :   '(possible programming error).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_CHXNM', STATUS )

      END
