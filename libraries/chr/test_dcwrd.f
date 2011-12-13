      SUBROUTINE TEST_DCWRD(STATUS)
*+
*  Name:
*     TEST_DCWRD

*  Purpose:
*     Test CHR_DCWRD.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_DCWRD(STATUS)

*  Description:
*     Test CHR_DCWRD.
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
*     01-MAR-1994 (ACC)
*        Second modularised version: broken further into one routine for
*        each of subroutine tested.  This subroutine created.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Subprograms called:
*     CHR_DCWRD

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
      INTEGER I, J,              ! INTEGER values
     :        ISTAT,             ! Local status
     :        PTRS(4),PTRE(4)    ! String indexes of Start/End of words
      CHARACTER*10 CHARS         ! Strings
      CHARACTER*5 WORDS(3)

*.

      ISTAT = SAI__OK

*    Test CHR_DCWRD

      CHARS = 'A z09  !@#'
      CALL CHR_DCWRD (CHARS, 2, I, PTRS, PTRE, WORDS, J)
      IF (.NOT. ((I .EQ. 2) .AND. (J .EQ. SAI__ERROR) .AND.
     :    (PTRS(1) .EQ. 1) .AND. (PTRS(2) .EQ. 3) .AND.
     :    (WORDS(1)(1:PTRE(1)-PTRS(1)+1) .EQ. 'A') .AND.
     :    (WORDS(2)(1:PTRE(2)-PTRS(2)+1) .EQ. 'z09'))) THEN
         PRINT *, 'CHR_DCWRD FAILS'
         PRINT *, 'NUMWRD = ', I
         PRINT *, 'LSTAT = ', J
         PRINT *, 'START = ', PTRS
         PRINT *, 'STOP = ', PTRE
         PRINT *, 'WORDS are ', WORDS
         ISTAT = SAI__ERROR
      ENDIF

      CHARS = 'A z09  !@#'
      CALL CHR_DCWRD (CHARS, 3, I, PTRS, PTRE, WORDS, J)
      IF (.NOT. ((I .EQ. 3) .AND. (J .EQ. SAI__OK) .AND.
     :    (PTRS(1) .EQ. 1) .AND. (PTRS(2) .EQ. 3) .AND.
     :    (WORDS(1)(1:PTRE(1)-PTRS(1)+1) .EQ. 'A') .AND.
     :    (WORDS(2)(1:PTRE(2)-PTRS(2)+1) .EQ. 'z09'))) THEN
         PRINT *, 'CHR_DCWRD FAILS'
         PRINT *, 'NUMWRD = ', I
         PRINT *, 'LSTAT = ', J
         PRINT *, 'START = ', PTRS
         PRINT *, 'STOP = ', PTRE
         PRINT *, 'WORDS are ', WORDS
         ISTAT = SAI__ERROR
      ENDIF

      CHARS = 'A z09     '
      CALL CHR_DCWRD (CHARS, 2, I, PTRS, PTRE, WORDS, J)
      IF (.NOT. ((I .EQ. 2) .AND. (J .EQ. SAI__OK) .AND.
     :    (PTRS(1) .EQ. 1) .AND. (PTRS(2) .EQ. 3) .AND.
     :    (WORDS(1)(1:PTRE(1)-PTRS(1)+1) .EQ. 'A') .AND.
     :    (WORDS(2)(1:PTRE(2)-PTRS(2)+1) .EQ. 'z09'))) THEN
         PRINT *, 'CHR_DCWRD FAILS'
         PRINT *, 'NUMWRD = ', I
         PRINT *, 'LSTAT = ', J
         PRINT *, 'START = ', PTRS
         PRINT *, 'STOP = ', PTRE
         PRINT *, 'WORDS are ', WORDS
         ISTAT = SAI__ERROR
      ENDIF

      IF ( ISTAT .EQ. SAI__OK ) THEN
         PRINT *, 'CHR_DCWRD OK'
      ELSE
         STATUS = SAI__ERROR
      ENDIF

      END
