      SUBROUTINE NDF1_FILEX( FILE, MODE, REPORT, OK, STATUS )
*+
*  Name:
*     NDF1_FILEX

*  Purpose:
*     Determine if a file exists and is accessible.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_FILEX( FILE, MODE, REPORT, OK, STATUS )

*  Description:
*     The routine determines whether a file exists and (optionally)
*     whether it is accessible using a specified access mode, and
*     returns a logical result. If the file does not exist or is not
*     accessible, the routine will optionally set STATUS and report an
*     appropriate error.

*  Arguments:
*     FILE = CHARACTER * ( * ) (Given)
*        Name of the file.
*     MODE = CHARACTER * ( * ) (Given)
*        The required mode of access: 'READ', 'UPDATE' or 'WRITE' (case
*        insensitive). If an existence test only is required, then this
*        argument should be blank.
*     REPORT = LOGICAL (Given)
*        Whether to set STATUS and report an error if the file does not
*        exist or is not accessible (.TRUE. ==> report an error, .FALSE.
*        ==> return without further action).
*     OK = LOGICAL (Returned)
*        Whether the file exists and is accessible.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     21-OCT-1993 (RFWS):
*        Original version.
*     29-APR-1994 (RFWS):
*        Allow MODE to appear in error messages, if it is supplied.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      CHARACTER * ( * ) FILE
      CHARACTER * ( * ) MODE
      LOGICAL REPORT

*  Arguments Returned:
      LOGICAL OK

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IERR               ! I/O error code

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Enquire whether the file exists, checking for errors.
      INQUIRE( FILE = FILE, EXIST = OK, IOSTAT = IERR )
      IF ( IERR .NE. 0 ) THEN
         STATUS = NDF__INQER
         CALL MSG_SETC( 'FILE', FILE )
         CALL ERR_FIOER( 'MESSAGE', IERR )
         CALL ERR_REP( 'NDF1_FILEX_INQ',
     :        'Error enquiring about the existence of the file ' //
     :        '''^FILE'' - ^MESSAGE', STATUS )

*  If it exists, and MODE is blank, then there is nothing more to do.
*  Otherwise, determine whether the required mode of access is
*  available.
      ELSE IF ( OK ) THEN
         IF ( MODE .NE. ' ' ) THEN
            CALL NDF1_FILAC( FILE, MODE, REPORT, OK, STATUS )
         END IF

*  If it does not exist, then report an error, if required.
      ELSE IF ( REPORT ) THEN
         STATUS = NDF__FILNF

*  Include the access mode string only if supplied.
         CALL MSG_SETC( 'FILE', FILE )
         IF ( MODE .NE. ' ' ) THEN
            CALL MSG_SETC( 'MODE', MODE )
            CALL ERR_REP( 'NDF1_FILEX_ERR1',
     :           'Unable to open the file ''^FILE'' for ^MODE ' //
     :           'access; file does not exist.', STATUS )
         ELSE
            CALL ERR_REP( 'NDF1_FILEX_ERR2',
     :           'Unable to open the file ''^FILE''; file does not ' //
     :           'exist.', STATUS )
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_FILEX', STATUS )

      END
