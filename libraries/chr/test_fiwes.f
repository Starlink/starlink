      SUBROUTINE TEST_FIWES(STATUS)
*+
*  Name:
*     TEST_FIWES

*  Purpose:
*     Test CHR_FIWE and CHR_FIWS.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_FIWES(STATUS)

*  Description:
*     Test CHR_FIWE and CHR_FIWS.
*     If any failure occurs, return STATUS = SAI__ERROR.
*     Otherwise, STATUS is unchanged.

*  Arguments:
*     STATUS = INTEGER (Returned)
*        The status of the tests.

*  Copyright:
*     Copyright (C) 1989, 1993, 1994 Science & Engineering Research Council.
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
*     RLVAD::AJC: A J Chipperfield (STARLINK)
*     RLVAD::ACC: A C Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-AUG-1989 (RLVAD::AJC):
*        Original version.
*     14-SEP-1993 (ACC)
*        Modularised version: broken into one routine for each of 5 main
*        categories of tests.
*     02-MAR-1994 (ACC)
*        Second modularised version: broken further into one routine for
*        each of subroutine tested.  This subroutine created.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Subprograms called:
*     CHR_FIWE, CHR_FIWS

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
*     None

*  Arguments Returned:
      INTEGER STATUS

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CHR_ERR'

*  Local Variables:
      INTEGER ISTAT              ! Local status
      INTEGER I, J,              ! INTEGER values
     :        PTRS(4),PTRE(4)    ! String indexes of Start/End of words
      CHARACTER*10 CHARS         ! Strings

*.

*    Test CHR_FIWS, CHR_FIWE

*    Repeat the CHR_DCWRD test using CHR_FIWS and CHR_FIWE
*    and with a comma as a word terminator.
      CHARS = 'A z09, !@#'
      I = 1
      J = 0
      ISTAT = SAI__OK
      DOWHILE ((ISTAT .EQ. SAI__OK) .AND. (I .LE. 4))
         PTRS(I) = 0
         PTRE(I) = 0
*      Find start of Ith word
         CALL CHR_FIWS ( CHARS, J, ISTAT)
         IF (ISTAT .EQ. SAI__OK) THEN
            PTRS(I) = J

*        Find end of Ith word
           CALL CHR_FIWE (CHARS, J, ISTAT)
           PTRE(I) = J
*        Move off word
           J = J + 1
*        Do next word
           I = I + 1

         ENDIF

      ENDDO

      IF ((ISTAT .EQ. CHR__ENDOFSENT) .AND.
     :    (I .EQ. 4) .AND. (J .EQ. 11) .AND.
     :    (CHARS(PTRS(1):PTRE(1)) .EQ. 'A') .AND.
     :    (CHARS(PTRS(2):PTRE(2)) .EQ. 'z09') .AND.
     :    (CHARS(PTRS(3):PTRE(3)) .EQ. '!@#')) THEN
         PRINT *, 'CHR_FIWS/FIWE OK'
      ELSE
         PRINT *, 'CHR_FIWS/FIWE FAILURE'
         PRINT *, 'START = ', PTRS
         PRINT *, 'STOP = ', PTRE
         PRINT *, 'STRING is ', CHARS
         PRINT *, 'STATUS is ', ISTAT
      ENDIF

*    Repeat the CHR_DCWRD test AGAIN using CHR_FIWS and CHR_FIWE
*    and with a comma as a word terminator.
      CHARS = 'A z09,   !'
      I = 1
      J = 0
      ISTAT = SAI__OK
      DOWHILE ((ISTAT .EQ. SAI__OK) .AND. (I .LE. 4))
         PTRS(I) = 0
         PTRE(I) = 0
*      Find start of Ith word
         CALL CHR_FIWS ( CHARS, J, ISTAT)
         IF (ISTAT .EQ. SAI__OK) THEN
            PTRS(I) = J

*        Find end of Ith word
           CALL CHR_FIWE (CHARS, J, ISTAT)
           PTRE(I) = J
*        Move off word
           J = J + 1
*        Do next word
           I = I + 1

         ENDIF

      ENDDO

      IF ((ISTAT .EQ. CHR__ENDOFSENT) .AND.
     :    (I .EQ. 4) .AND. (J .EQ. 11) .AND.
     :    (CHARS(PTRS(1):PTRE(1)) .EQ. 'A') .AND.
     :    (CHARS(PTRS(2):PTRE(2)) .EQ. 'z09') .AND.
     :    (CHARS(PTRS(3):PTRE(3)) .EQ. '!')) THEN
         PRINT *, 'CHR_FIWS/FIWE OK'
      ELSE
         PRINT *, 'CHR_FIWS/FIWE FAILURE'
         PRINT *, 'START = ', PTRS
         PRINT *, 'STOP = ', PTRE
         PRINT *, 'STRING is ', CHARS
         PRINT *, 'STATUS is ', ISTAT
      ENDIF

*    Repeat the CHR_DCWRD test using CHR_FIWS and CHR_FIWE
*    and with a comma as a word terminator.
      CHARS = 'A z09,  ! '
      I = 1
      J = 0
      ISTAT = SAI__OK
      DOWHILE ((ISTAT .EQ. SAI__OK) .AND. (I .LE. 4))
         PTRS(I) = 0
         PTRE(I) = 0
*      Find start of Ith word
         CALL CHR_FIWS ( CHARS, J, ISTAT)
         IF (ISTAT .EQ. SAI__OK) THEN
            PTRS(I) = J

*        Find end of Ith word
           CALL CHR_FIWE (CHARS, J, ISTAT)
           PTRE(I) = J
*        Move off word
           J = J + 1
*        Do next word
           I = I + 1

         ENDIF

      ENDDO

      IF ((ISTAT .EQ. CHR__WNOTF) .AND.
     :    (I .EQ. 4) .AND. (J .EQ. 10) .AND.
     :    (CHARS(PTRS(1):PTRE(1)) .EQ. 'A') .AND.
     :    (CHARS(PTRS(2):PTRE(2)) .EQ. 'z09') .AND.
     :    (CHARS(PTRS(3):PTRE(3)) .EQ. '!')) THEN
         PRINT *, 'CHR_FIWS/FIWE OK'
      ELSE
         PRINT *, 'CHR_FIWS/FIWE FAILURE'
         PRINT *, 'START = ', PTRS
         PRINT *, 'STOP = ', PTRE
         PRINT *, 'STRING is ', CHARS
         PRINT *, 'STATUS is ', ISTAT
      ENDIF

      END
