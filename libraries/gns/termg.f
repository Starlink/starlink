      SUBROUTINE gns_1TERMG(STATUS)
*+
*  Name:
*     gns_1TERMG

*  Purpose:
*     Close the GNS system for the GKS package

*  Language:
*     {routine_language}

*  Invocation:
*     CALL GNS_1TERMG(STATUS)

*  Description:
*     The GKS devices data file is closed

*  Arguments:
*     STATUS = INTEGER (Given & Returned)

*  Copyright:
*     Copyright (C) 1989 Science & Engineering Research Council. All
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
*     16-MAY-1989 (DLT):
*        Original.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      IMPLICIT NONE

      INCLUDE 'GNS_PAR'
      INCLUDE 'gns.cmn'

      INTEGER STATUS

      LOGICAL OPEN

      INQUIRE ( UNIT = LUNGNS, OPENED = OPEN)
      IF (OPEN) CLOSE (UNIT=LUNGNS)

      END
