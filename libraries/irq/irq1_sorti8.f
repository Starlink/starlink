      SUBROUTINE IRQ1_SORTI8( SIZE, DATA, STATUS )
*+
*  Name:
*     IRQ1_SORTI8

*  Purpose:
*     Sorts integers into ascending order.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ1_SORTI8( SIZE, DATA, STATUS )

*  Description:
*     Uses a simple "Bubble Sort" method.

*  Arguments:
*     SIZE = INTEGER*8 (Given)
*        Number of integers to sort.
*     DATA( SIZE ) = INTEGER*8 (Given and Returned)
*        Integers to be sorted. On exit, they are sorted so that the
*        smallest value occurs at DATA(1) and the largest at DATA(SIZE).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
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
*     30-JUL-1991 (DSB):
*        Original version.
*     24-OCT-2019 (DSB):
*        Changed to use 8-byte size.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER*8 SIZE

*  Arguments Given and Returned:
      INTEGER*8 DATA( SIZE )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER*8   ADJEL          ! The element adjacent to the current
                                 ! element.
      LOGICAL   DONE             ! True when the elements of array DATA
                                 ! are in increasing order.
      INTEGER*8   ELEMNT         ! The current element of the array
                                 ! being checked for correct order.
      INTEGER*8   LASTEL         ! Points to the last element of the
                                 ! array which needs to be checked for
                                 ! being in order.
      INTEGER*8   TMP            ! Temporary storage.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialize local variables.
      LASTEL = SIZE
      DONE = .FALSE.

*  Loop until data is in correct order.
      DO WHILE( .NOT.DONE )

*  On each pass through the data the highest number gets 'washed' down
*  to the end of the array, therefore it is not neccessary to check the
*  previously last element because it is known to be in the right order.
         LASTEL = LASTEL - 1

*  Go through all the data considering adjacent pairs. If a pair is in
*  the wrong order swap them round.
         DONE = .TRUE.
         DO ELEMNT = 1, LASTEL
            ADJEL = ELEMNT + 1
            IF( DATA( ELEMNT ) .GT. DATA( ADJEL ) ) THEN
               TMP = DATA( ELEMNT )
               DATA( ELEMNT ) = DATA( ADJEL )
               DATA( ADJEL ) = TMP
               DONE = .FALSE.
            ENDIF
         ENDDO

*  Loop round for next pass through data.
      ENDDO

      END
