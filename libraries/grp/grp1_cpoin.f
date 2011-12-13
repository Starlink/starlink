      SUBROUTINE GRP1_CPOIN( POINT, STATUS )
*+
*  Name:
*     GRP1_CPOIN

*  Purpose:
*     Convert a pointer to a UNIX character array descriptor into a
*     pointer to the area of mapped memory which holds the characters.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_CPOIN( POINT, STATUS )

*  Description:
*     UNIX does not use descriptors for character arrays, so just
*     return the supplied pointer unchanged.

*  Arguments:
*     POINT = INTEGER (Given and Returned)
*        On input, this is a pointer to a VMS character array
*        descriptor obtained using GRP1_CDESC. On exit, it is unchanged.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
*     18-AUG-1992 (DSB):
*        Original version
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given and Returned:
      INTEGER POINT

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Return immediately without doing anything.
      END
