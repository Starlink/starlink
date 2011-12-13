      LOGICAL FUNCTION gns_FILTI(TYPE)
*+
*  Name:
*     GNS_FILTI

*  Purpose:
*     Dummy IDI workstation filter routine.

*  Language:
*     Starlink Fortran 77

*  Description:
*     Dummy IDI workstation filter routine; all workstation types are
*     accepted.

*  Arguments:
*     TYPE = CHAR (Given)
*         Workstation type

*  Returned Value:
*     gns_FILTI = LOGICAL (Returned)
*         Include in list

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     {original_author_entry}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      IMPLICIT NONE

      CHARACTER*(*) TYPE

      gns_FILTI = .TRUE.

      END
