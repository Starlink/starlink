      SUBROUTINE GRP1_CDESC( LEN, POINT, STATUS )
*+
*  Name:
*     GRP1_CDESC

*  Purpose:
*     Convert a pointer to an area of mapped memory into a pointer to a
*     UNIX character array descriptor.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_CDESC( LEN, POINT, STATUS )

*  Description:
*     UNIX does not use descriptors, so just return the pointer
*     unchanged.

*  Arguments:
*     LEN = INTEGER (Given)
*        The number of characters in each element of the array.
*     POINT = INTEGER (Given and Returned)
*        On input, this is a pointer to an area of memory mapped using
*        On exit, it is unchanged.
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

*  Arguments Given:
      INTEGER LEN

*  Arguments Given and Returned:
      INTEGER POINT

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Return without doing anything.
      END
