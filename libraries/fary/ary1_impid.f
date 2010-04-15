      SUBROUTINE ARY1_IMPID( IARY, IACB, STATUS )
*+
*  Name:
*     ARY1_IMPID

*  Purpose:
*     Import an identifier.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_IMPID( IARY, IACB, STATUS )

*  Description:
*     The routine converts an array identifier, previously issued by
*     ARY1_EXPID, into an index to the appropriate entry in the ACB.
*     The identifier value is fully checked and an error is reported if
*     it is not valid.

*  Arguments:
*     IARY = INTEGER (Given)
*        Array identifier.
*     IACB = INTEGER (Returned)
*        Index to an entry in the ACB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*      -  Set an initial value for the IACB argument before checking
*      the inherited status.
*     -  Decode the IARY value into an index for the ACB.
*     -  If a valid index was not obtained, then report an error.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-JUN-1989 (RFWS):
*        Original version.
*     16-AUG-1989 (RFWS):
*        Changed to call ARY1_ID2AC.
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
      INTEGER IARY

*  Arguments Returned:
      INTEGER IACB

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Set an initial value for the IACB argument.
      IACB = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert the identifier to an ACB index.
      CALL ARY1_ID2AC( IARY, IACB )


*  If a valid index was not returned, then report an error.
      IF ( IACB .LE. 0 ) THEN
         STATUS = ARY__IDINV
         CALL MSG_SETI( 'IARY', IARY )
         CALL ERR_REP( 'ARY1_IMPID_IDIN',
     :   'Array identifier invalid; its value is ^IARY (possible ' //
     :   'programming error).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_IMPID', STATUS )

      END
