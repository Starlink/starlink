************************************************************************

      SUBROUTINE AGI_1INIT

*+
*  Name:
*     AGI_1INIT

*  Purpose:
*     Initialise the dynamic cache.

*  Language:
*     VAX Fortran

*  Type of Module:
*     Subroutine

*  Invocation:
*     CALL AGI_1INIT

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Initialise the dynamic cache.

*  Algorithm:
*     For every record in the cache
*        Indicate the element of the FIFO array to be empty = -1.
*        Make the forward and inverse transformation numbers = 0.
*     Enddo
*     Initialise the FIFO pointer to be FIFLEN - 1.

*  Copyright:
*     Copyright (C) 1988, 1989, 1990, 1993 Science & Engineering Research Council.
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
*     July 1988
*     June 1989  Allowed for the increased number of FIFO's
*     August 1990  Initialise number of pictures
*     January 1993  Initialise CHEAD
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'AGI_PAR'


*  Global Variables:
      INCLUDE 'agi_cache'


*  Local Variables:
      INTEGER I, J

*.


*   Initialise the number of pictures and the workstation flag
      CNUMPS = 0
      CNUMPW = ' '

*   Initialise the header control block
      CHEAD = -1

*   Initialise each fifo in turn
      DO J = 0, NFIFO - 1

*   Initialise the FIFO array to -1
         DO I = 0, FIFLEN - 1
            FIFO( I, J ) = -1

*   Make sure that the transformation numbers are zero
            CTRFOR( I, J ) = 0
            CTRINV( I, J ) = 0

         ENDDO

*   Initialise the pointer to FIFLEN - 1,
*   so it gets changed to 0 on first pass
         PFIFO( J ) = FIFLEN - 1

      ENDDO

      END

