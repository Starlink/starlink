      SUBROUTINE gns_1HOSTN( NAME, LNAME, STATUS)
*+
*  Name:
*     gns_1HOSTN

*  Purpose:
*     returns the name of the system

*  Language:
*     Starlink Fortran 77

*  Description:
*     Gets the system name; eg the DECnet node name or IP host name

*  Arguments:
*     NAME = CHAR (Returned)
*         The system name
*     LNAME = INTEGER (Returned)
*         The length of the name
*     STATUS = INTEGER (Given & Returned)
*         Status

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     DLT: D L Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     06-JUN-1991 (DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      IMPLICIT NONE

      CHARACTER*(*) NAME
      INTEGER LNAME, STATUS

      INCLUDE 'SAE_PAR'

      INTEGER CHR_LEN
      CHARACTER*1 SYSNAM, REL, VER, MACH

      IF (STATUS.EQ.SAI__OK) THEN

*      Get the node name (all the other stuff is ignored)
         CALL PSX_UNAME(SYSNAM , NAME, REL, VER, MACH, STATUS)

*      Find the length of the string
         IF (STATUS.EQ.SAI__OK) THEN
            LNAME = CHR_LEN( NAME )
         ELSE
            LNAME = 0
         END IF

      END IF

      END
