      SUBROUTINE ATL_FRVFS( VFS, STATUS )
*+
*  Name:
*     ATL_FRVFS

*  Purpose:
*     Free a VFS, such as returned by ATL_GTVFS

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL_FRVFS( VFS, STATUS )

*  Description:
*     Free a virtual file structure, such as returned by ATL_GTVFS.

*  Arguments:
*     VFS = INTEGER (Returned)
*        A pointer to a virtual file structure (see atl2.c). A value of
*        zero is returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2019 East Asian Observatory.
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
*     DSB: David Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     16-SEP-2019 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given and Returned:
      INTEGER VFS

*  Status:
      INTEGER STATUS             ! Global status

*.

      CALL ATL2_DELET( VFS, STATUS )

      END
