      SUBROUTINE KPS1_MEMTR( N, FROM, IN, TO, OUT )
*+
*  Name:
*     KPS1_MEMTR

*  Purpose:
*     Transfers data from one array to another.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_MEMTR( N, FROM, IN, TO, OUT )

*  Description:
*     Transfers N elements from the vector IN, starting with element
*     FROM, to the vector OUT, starting at element TO.

*  Arguments:
*     N = INTEGER (Given)
*        Number of elements to transfer.
*     FROM = INTEGER (Given)
*        Index of first element of IN to be copied.
*     IN( * ) = REAL (Given)
*        The input array.
*     TO = INTEGER (Given)
*        The index at which the first transferred element should be
*        stored in OUT.
*     OUT( * ) = REAL (Given and Returned)
*        The output array.

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
*     20-MAR-1995 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Arguments Given:
      INTEGER N
      INTEGER FROM
      REAL IN( * )
      INTEGER TO

*  Arguments Given and Returned:
      REAL OUT( * )

*  Local Variables:
      INTEGER OFFSET
*.

      OFFSET = TO - FROM

      DO I = FROM, FROM + N - 1
         OUT( OFFSET + I ) = IN( I )
      END DO

      END
