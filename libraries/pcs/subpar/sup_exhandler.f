      SUBROUTINE SUBPAR_EXHANDLER ( STATUS )
*+
*  Name:
*     SUBPAR_EXHANDLER

*  Purpose:
*     Exit handler for parameter system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_EXHANDLER ( STATUS )

*  Description:
*     The exit handler for the parameter system. This routine is called
*     by VMS on image exit. It simply closes HDS.

*  Arguments:
*     STATUS=INTEGER

*  Algorithm:
*     Just call the HDS closedown routine.

*  Copyright:
*     Copyright (C) 1984 Science & Engineering Research Council.
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
*     BDK: B D Kelly (ROE)
*     {enter_new_authors_here}

*  History:
*     25-OCT-1984 (BDK):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'


*  Status:
      INTEGER STATUS


*  Local Variables:
      INTEGER ISTAT

*.


      ISTAT = SAI__OK
      CALL HDS_STOP (ISTAT)

      END
