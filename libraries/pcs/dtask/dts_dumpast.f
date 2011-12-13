      SUBROUTINE DTASK_DUMPAST ( ASTPARM )
*+
*  Name:
*     DTASK_DUMPAST

*  Purpose:
*     Dummy for Unix: AST routine for generating a task stack dump

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     An AST routine, invoked by VMS.

*  Description:
*     Generate a stack dump of a task, then reenable the dumping
*     facility.

*  Arguments:
*     ASTPARM=INTEGER (given)
*           the AST parameter. Unused.

*  Algorithm:
*     Call LIB$SIGNAL to generate a stack dump. Then call DTASK_SETDUMP
*     to reenable the AST.

*  Copyright:
*     Copyright (C) 1986, 1991 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     13-AUG-1986 (REVAD::BDK):
*        Original
*     27-AUG-1986 (REVAD::BDK):
*        Use DTASK__DUMP in the signal
*     25-APR-1991 (REVAD::BDK):
*        Revise INCLUDE files
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DTASK_ERR'

*  Arguments Given:
      INTEGER ASTPARM     ! the AST parameter.

*  Local Variables:
      INTEGER STATUS
*.

!      CALL LIB$SIGNAL ( %VAL(DTASK__DUMP) )

      STATUS = SAI__OK
      CALL DTASK_SETDUMP ( STATUS )

      END
