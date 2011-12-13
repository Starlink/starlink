      SUBROUTINE IRA1_CHECK( IDA, STATUS )
*+
*  Name:
*     IRA1_CHECK

*  Purpose:
*     CHeck that the identifier is OK and that IRA has been initialised.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_CHECK( IDA, STATUS )

*  Description:
*     A check is made that IRA has been initialised. If not an error is
*     reported, otherwise a check is made that the supplied IRA
*     identifier is valid. An error is report if it is not.

*  Arguments:
*     IDA = INTEGER (Given)
*        The IRA identifier to be checked.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
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
*     22-JAN-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRA_ERR'          ! IRA error constants.

*  Global Variables:
      INCLUDE 'IRA_COM'          ! IRA common blocks.
*        ACM_VALID( IRA__MAX ) = LOGICAL (Read)
*           If true, then the associated elements of the other arrays
*           held in common contain valid astrometry information.
*        ACM_STATE = CHARACTER (Read)
*           Set to the value of symbolic constant IRA__GOING to indicate
*           that IRA has been initialised.

*  Arguments Given:
      INTEGER IDA

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that IRA has been initialised.
      IF( ACM_STATE .NE. IRA__GOING ) THEN
         STATUS = IRA__INIT
         CALL ERR_REP( 'IRA1_CHECK_ERR1',
     :             'IRA1_CHECK: The IRAS90 astrometry system has not '//
     :             'been initialised', STATUS )
         GO TO 999
      END IF

*  Check the supplied IRA identifier is valid.
      IF( IDA .LT. 0 .OR. IDA .GT. IRA__MAX .OR. IDA .EQ. IRA__NOID )
     :                                                              THEN
         STATUS = IRA__INVID
      ELSE
         IF( .NOT. ACM_VALID( IDA ) ) STATUS = IRA__INVID
      END IF

      IF( STATUS .EQ. IRA__INVID ) THEN
         CALL ERR_REP( 'IRA1_CHECK_ERR1',
     :                 'IRA1_CHECK: Invalid IRA identifier supplied',
     :                 STATUS )
      END IF

 999  CONTINUE

      END
