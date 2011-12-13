      SUBROUTINE IRA1_GETID( IDA, STATUS )
*+
*  Name:
*     IRA1_GETID

*  Purpose:
*     Obtain a free IRA identifier.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_GETID( IDA, STATUS )

*  Description:
*     The lowest free IRA identifier is returned. If no free identifiers
*     can be found then an error is reported and status is returned
*     equal to IRA__NOMOR. Note, this routine does NOT flag the
*     returned identifier as used, in common.

*  Arguments:
*     IDA = INTEGER (Returned)
*        The lowest free IRA identifier. If an error occurs IDA is
*        returned equal to the value IRA__NOID.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-JAN-1991 (DSB):
*        Original version.
*     26-APR-1991 (DSB):
*        Modified for IRA version 2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRA_ERR'          ! IRA errors.

*  Global Variables:
      INCLUDE 'IRA_COM'          ! IRA common blocks.
*        ACM_VALID( IRA__MAX ) = LOGICAL (Read)
*           True if the elements of the corresponding elements of
*           all the common arrays contain valid astrometry information.

*  Arguments Returned:
      INTEGER IDA

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop count.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the identifier to the invalid value, IRA__NOID.
      IDA = IRA__NOID

*  Set the identifier to the lowest invalid identifier found.
      DO I = 1, IRA__MAX
         IF( .NOT.ACM_VALID (I) .AND. IDA .EQ. IRA__NOID) IDA = I
      END DO

*  If no invalid identifier was found, give an error.
      IF( IDA .EQ. IRA__NOID ) THEN
         STATUS = IRA__NOMOR
         CALL ERR_REP( 'IRA1_GETID_ERR1',
     :        'IRA1_GETID: No room for any more astrometry information',
     :                 STATUS )
      END IF

      END
