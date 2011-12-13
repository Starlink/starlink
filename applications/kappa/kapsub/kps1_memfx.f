      SUBROUTINE KPS1_MEMFX( SIZE, LAST, DATA )
*+
*  Name:
*     KPS1_MEMFX

*  Purpose:
*     Fixes an external MEMSYS3 area.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_MEMFX( SIZE, LAST, DATA )

*  Description:
*     Fills the section of an external area which lies beyond the end
*     of the actual image with zeros.

*  Arguments:
*     SIZE = INTEGER (Given)
*        Total number of elements in the data file.
*     LAST = INTEGER (Given)
*        The index of the last genuine data value.
*     DATA( SIZE ) = REAL (Given and Returned)
*        The data array.  Elements with indices higher than LAST will
*        be set to zero on return.

*  Copyright:
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-MAR-1995 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Arguments Given:
      INTEGER SIZE
      INTEGER LAST

*  Arguments Given and Returned:
      REAL DATA( SIZE )

*  Local Variables:
      INTEGER I

*.

      DO I = LAST + 1, SIZE
         DATA( I ) = 0.0
      END DO

      END
