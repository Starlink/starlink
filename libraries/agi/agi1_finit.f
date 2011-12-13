************************************************************************

      SUBROUTINE AGI_1FINIT ( FRELEN, FRELIS, NEXFRE )

*+
*  Name:
*     AGI_1FINIT

*  Purpose:
*     Initialise a free list.

*  Language:
*     VAX Fortran

*  Type of Module:
*     Subroutine

*  Invocation:
*     CALL AGI_1FINIT( FRELEN, FRELIS, NEXFRE )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Initialise a free list so that NEXFRE and FRELIS indicate the
*     sequence of elements from 1 to FRELEN. The free list is terminated
*     with a -1 to indicate no more free elements. The initialisation is
*     as follows
*              --------------          -----------------
*              | NEXFRE | 1 |          | FRELIS(1) | 2 |
*              --------------          -----------------
*                                      | FRELIS(2) | 3 |
*                                      -----------------
*                                      .               .
*                                      ------------------
*                                      | FRELIS(N) | -1 |
*                                      ------------------

*  Copyright:
*     Copyright (C) 1988 Science & Engineering Research Council.
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
*     Nick Eaton  ( DUVAD::NE )
*     {enter_new_authors_here}

*  History:
*     Aug 1988
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*

*  Type Definitions:
      IMPLICIT NONE


*  Arguments Given:
*     Length of the array containing the free list.
      INTEGER FRELEN


*  Arguments Returned:
*     Array containing the free list
      INTEGER FRELIS( FRELEN )

*     Pointer indicating next free in free list array
      INTEGER NEXFRE


*  Local Constants:
      INTEGER I

*.


*   Make NEXFRE point to first member of free list
      NEXFRE = 1

*   Make sequence in free list array. Terminate with -1.
      DO I = 1, FRELEN - 1
         FRELIS( I ) = I + 1
      ENDDO
      FRELIS( FRELEN ) = -1

      END

