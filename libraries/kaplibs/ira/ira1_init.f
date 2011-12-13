      BLOCK DATA IRA1_INIT
*+
*  Name:
*     IRA1_INIT

*  Purpose:
*     Initialise the IRA common blocks.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     BLOCK DATA

*  Description:
*     All IRA identifiers are set un-used. The state of IRA is set to
*     STOPPED. Initial values for graphics options are set up.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
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
*     26-AUG-1992 (DSB):
*        Original version
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'DAT_PAR'          ! DAT constants.
      INCLUDE 'IRA_PAR'          ! IRA public constants.
      INCLUDE 'PRM_PAR'          ! Starlink data constant.

*  Global Variables:
      INCLUDE 'IRA_COM'          ! IRA common blocks.

*  Local Variables:
      INTEGER I                  ! Implicit loop count.

*  Global Data:
*  Initalise all IRA identifiers to invalid.
      DATA (ACM_VALID( I ), I = 1, IRA__MAX) / IRA__MAX*.FALSE. /

*  Initalise the state to STOPPED
      DATA ACM_STATE/'STOPPED'/

*  Initialise graphics options.
      DATA ACM_DROPT/ 1.25D-2, 1.25D-2, 6.0D0, 1.0D0, -1.0D0, -1.0D0,
     :                1.0D0, 1.0D0, -1.0D0, -1.0D0, 1.0D0, 1.0D0 /

*  Indicate that no curve has yet been drawn for which IRA_DRBRK can
*  return information.
      DATA ACM_LENG/ VAL__BADR /

*  Indicate that no value has yet been drawn for which IRA_DRVPO can
*  return information.
      DATA ACM_DRVPO/ 5*VAL__BADR /
*.

      END
