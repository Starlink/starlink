      SUBROUTINE ARY1_CHSCN( NAME, STATUS )
*+
*  Name:
*     ARY1_CHSCN

*  Purpose:
*     Check for a standard data component name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_CHSCN( NAME, STATUS )

*  Description:
*     The routine checks that the data component name supplied conforms
*     to the standard form for naming Starlink data structure
*     components; i.e. that it is not blank, is no more than DAT__SZNAM
*     characters long, begins with an alphabetic character and
*     continues with alphanumeric characters (including underscore)
*     only. If the name is non-standard, then an error is reported.
*     Otherwise, the routine returns without further action.

*  Arguments:
*     CHARACTER * ( * ) = NAME (Given)
*        The data component name to be checked.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Report an error if any of the constraints on standard component
*     names is violated.

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
*     14-SEP-1989 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Arguments Given:
      CHARACTER * ( * ) NAME

*  External References:
      INTEGER CHR_LEN            ! Significant length of string
      LOGICAL CHR_ISNAM          ! Whether string is a valid name

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check whether all the constraints on component names are satisfied.
      IF ( ( NAME .EQ. ' ' ) .OR.
     :     ( CHR_LEN( NAME ) .GT. DAT__SZNAM ) .OR.
     :     ( .NOT. CHR_ISNAM( NAME ) ) ) THEN

*  If not, then report an error.
         STATUS = ARY__NSDCN
         CALL MSG_SETC( 'NAME', NAME )
         CALL ERR_REP( 'ARY1_CHSCN_BAD',
     :   'Non-standard data component name ''^NAME'' specified ' //
     :   '(possible programming error).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_CHSCN', STATUS )

      END
