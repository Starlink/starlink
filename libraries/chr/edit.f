      SUBROUTINE TEST_EDIT(STATUS)
*+
*  Name:
*     TEST_EDIT

*  Purpose:
*     Test the string editting routines.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_EDIT(STATUS)

*  Description:
*     Test each of the string editting routines listed in Appendix A.3
*     of SUN/40.3.
*     If any failure occurs, return STATUS = SAI__ERROR.
*     Otherwise, STATUS is unchanged.

*  Arguments:
*     STATUS = INTEGER (Returned)
*        The status of the tests.

*  Copyright:
*     Copyright (C) 1993, 1994 Science & Engineering Research Council.
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
*     RLVAD::ACC: A C Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-SEP-1993 (ACC)
*        Original version.
*     02-MAR-1994 (ACC)
*        Broke into separate routines for each routine tested.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Subprograms called:
*     TEST_APPND, TEST_CLEAN, TEST_COPY, TEST_DCWRD, TEST_FILL,
*     TEST_LDBLK, TEST_LINBR, TEST_MOVE, TEST_PFORM, TEST_PREFX,
*     TEST_RJUST, TEST_RMBLK, TEST_RMCHR, TEST_SORT, TEST_SWAP,
*     TEST_TERM, TEST_TRCHR, TEST_TRUNC

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

*.

      PRINT *,' '
      PRINT *,'*** Test edit string routines ***'

      STATUS = SAI__OK

*    Test CHR_APPND

      ISTAT = SAI__OK
      CALL TEST_APPND(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_CLEAN

      ISTAT = SAI__OK
      CALL TEST_CLEAN(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_COPY

      ISTAT = SAI__OK
      CALL TEST_COPY(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_DCWRD

      ISTAT = SAI__OK
      CALL TEST_DCWRD(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_FILL

      ISTAT = SAI__OK
      CALL TEST_FILL(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_LDBLK

      ISTAT = SAI__OK
      CALL TEST_LDBLK(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_LINBR

      ISTAT = SAI__OK
      CALL TEST_LINBR(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_MOVE

      ISTAT = SAI__OK
      CALL TEST_MOVE(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_PFORM

      ISTAT = SAI__OK
      CALL TEST_PFORM(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_PREFX

      ISTAT = SAI__OK
      CALL TEST_PREFX(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_RJUST

      ISTAT = SAI__OK
      CALL TEST_RJUST(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_RMBLK

      ISTAT = SAI__OK
      CALL TEST_RMBLK(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_RMCHR

      ISTAT = SAI__OK
      CALL TEST_RMCHR(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_SORT

      ISTAT = SAI__OK
      CALL TEST_SORT(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_SWAP

      ISTAT = SAI__OK
      CALL TEST_SWAP(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_TERM

      ISTAT = SAI__OK
      CALL TEST_TERM(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_TRCHR

      ISTAT = SAI__OK
      CALL TEST_TRCHR(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_TRUNC

      ISTAT = SAI__OK
      CALL TEST_TRUNC(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Write summary message

      IF (STATUS .EQ. SAI__OK) THEN
         PRINT *,'*** All edit string routines OK ***'
      ELSE
         PRINT *,'*** Error(s) in edit string routines ***'
      END IF

      END
