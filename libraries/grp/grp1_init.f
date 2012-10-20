      BLOCK DATA GRP1_INIT
*+
*  Name:
*     GRP1_INIT

*  Purpose:
*     Initialise the GRP common blocks.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     BLOCK DATA

*  Description:
*     All GRP slot numbers are set un-used. The group expression
*     control character names are established.

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
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
*     18-AUG-1992 (DSB):
*        Original version
*     26-JAN-1994 (DSB):
*        OPEN_KERNEL and CLOSE_KERNEL control character names added.
*     27-AUG-1999 (DSB):
*        ESCAPE control character name added.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'GRP_CONST'        ! GRP private constants.
      INCLUDE 'GRP_PAR'          ! GRP public constants.
      INCLUDE 'GRP_ERR'          ! GRP error constants (including
                                 ! facility number)

*  Global Variables:
      INCLUDE 'GRP_COM'          ! GRP common blocks.
*        CMN_CCNAM( GRP__NCHAR ) = CHARACTER*(GRP__SZWRD) (Write)
*           The names which are used to refer to each control character.
*        CMN_IDCNT = INTEGER (Write)
*           Count of slot numbers issued so far.
*        CMN_IDCTX = INTEGER (Write)
*           Current slot number context level.
*        CMN_USED( GRP__MAXG ) = LOGICAL (Write)
*           True if the corresponding common array slot is in use.

*  Local Variables:
      INTEGER I                  ! Implicit loop count.

*  Global Data:
*  Indicate that no slots within the common arrays are currently used.
      DATA (CMN_USED( I ), I = 1, GRP__MAXG) / GRP__MAXG*.FALSE. /

*  Set the count of slot numbers issued so far to a random number
*  (actually the GRP facility number).
      DATA CMN_IDCNT / GRP__FAC /

*  Set the current slot number context level to 1.
      DATA CMN_IDCTX / 1 /

*  No group is currently being watched.
      DATA CMN_WATCH / GRP__NOID /

*  Set up the control character names.
      DATA (CMN_CCNAM( I ), I = 1, GRP__NCHAR) /
     : 'INDIRECTION',
     : 'COMMENT',
     : 'DELIMITER',
     : 'NAME_TOKEN',
     : 'SEPARATOR',
     : 'OPEN_NEST',
     : 'CLOSE_NEST',
     : 'FLAG',
     : 'NULL',
     : 'OPEN_KERNEL',
     : 'CLOSE_KERNEL',
     : 'ESCAPE' /

*.

      END
