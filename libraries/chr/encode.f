      SUBROUTINE TEST_ENCODE(STATUS)
*+
*  Name:
*     TEST_ENCODE

*  Purpose:
*     Test the encoding routines.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_ENCODE(STATUS)

*  Description:
*     Test each of the encoding routines listed in Appendix A.3
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
*     2012-05-03 (TIMJ):
*        Test CHR_KTOC
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Subprograms called:
*     TEST_DTOAN, TEST_DTOC, TEST_ITOB, TEST_ITOC, TEST_ITOH,
*     TEST_ITOO, TEST_LTOC, TEST_PUTC, TEST_PUTD, TEST_PUTI,
*     TEST_PUTL, TEST_PUTR, TEST_RTOAN, TEST_RTOC, TEST_KTOC
*

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
      PRINT *,'*** Test encode routines ***'

      STATUS = SAI__OK

*    Test CHR_DTOAN

      ISTAT = SAI__OK
      CALL TEST_DTOAN(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_DTOC

      ISTAT = SAI__OK
      CALL TEST_DTOC(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_ITOB

      ISTAT = SAI__OK
      CALL TEST_ITOB(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_ITOC

      ISTAT = SAI__OK
      CALL TEST_ITOC(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_KTOC

      ISTAT = SAI__OK
      CALL TEST_KTOC(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_ITOH

      ISTAT = SAI__OK
      CALL TEST_ITOH(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_ITOO

      ISTAT = SAI__OK
      CALL TEST_ITOO(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_LTOC

      ISTAT = SAI__OK
      CALL TEST_LTOC(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_PUTC

      ISTAT = SAI__OK
      CALL TEST_PUTC(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_PUTD

      ISTAT = SAI__OK
      CALL TEST_PUTD(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_PUTI

      ISTAT = SAI__OK
      CALL TEST_PUTI(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_PUTL

      ISTAT = SAI__OK
      CALL TEST_PUTL(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_PUTR

      ISTAT = SAI__OK
      CALL TEST_PUTR(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_RTOAN

      ISTAT = SAI__OK
      CALL TEST_RTOAN(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_RTOC

      ISTAT = SAI__OK
      CALL TEST_RTOC(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Write summary message

      IF (STATUS .EQ. SAI__OK) THEN
         PRINT *,'*** All encode routines OK ***'
      ELSE
         PRINT *,'*** Error(s) in encode routines ***'
      END IF

      END
