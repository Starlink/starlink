      SUBROUTINE IRA_CLOSE( STATUS )
*+
*  Name:
*     IRA_CLOSE

*  Purpose:
*     Closes down the IRA astrometry package.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_CLOSE( STATUS )

*  Description:
*     This routine should be called once IRA facilities are no longer
*     needed. It annuls any currently valid IRA identifiers, releasing
*     any resources reserved by them. Once this routine has been called,
*     any further use of IRA must be preceeded with a call to IRA_INIT.
*
*     This routine attempts to execute even if STATUS is set to a bad
*     value on entry.
*
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-JAN-1991 (DSB):
*        Original version.
*     27-APR-1991 (DSB):
*        Modified for IRA Version 2.
*     12-FEB-1993 (DSB):
*        Remove locators from common.
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

*  Global Variables:
      INCLUDE 'IRA_COM'          ! IRA common blocks.
*        ACM_STATE = CHARACTER (Write)
*           Set to the value of symbolic constant IRA__GOING if IRA
*           has been initialised.
*        ACM_VALID( IRA__MAX ) = LOGICAL (Write)
*           If true, then the associated elements of the other arrays
*           held in common contain valid astrometry information.

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop count.

*.

*  Indicate that IRA has been closed.
      ACM_STATE = ' '

*  Ensure all IRA identifiers are annulled.
      DO I = 1, IRA__MAX
         ACM_VALID( I ) = .FALSE.
      END DO

      END
