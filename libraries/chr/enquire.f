      SUBROUTINE TEST_ENQUIRE(STATUS)
*+
*  Name:
*     TEST_ENQUIRE

*  Purpose:
*     Test the enquire routines.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_ENQUIRE(STATUS)

*  Description:
*     Test each of the enquire routines listed in Appendix A.3 of
*     SUN/40.3.
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
*     TEST_NTH, TEST_INSET, TEST_ISALF, TEST_ISALM,
*     TEST_ISDIG, TEST_ISNAM, TEST_LEN, TEST_SIZE

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
      PRINT *,'*** Test enquire routines ***'

      STATUS = SAI__OK

*    Test CHR_NTH

      ISTAT = SAI__OK
      CALL TEST_NTH(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_INSET

      ISTAT = SAI__OK
      CALL TEST_INSET(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_ISALF

      ISTAT = SAI__OK
      CALL TEST_ISALF(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_ISALM

      ISTAT = SAI__OK
      CALL TEST_ISALM(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_ISDIG

      ISTAT = SAI__OK
      CALL TEST_ISDIG(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_ISNAM

      ISTAT = SAI__OK
      CALL TEST_ISNAM(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_LEN

      ISTAT = SAI__OK
      CALL TEST_LEN(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_SIZE

      ISTAT = SAI__OK
      CALL TEST_SIZE(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Write summary message

      IF (STATUS .EQ. SAI__OK) THEN
         PRINT *,'*** All enquire routines OK ***'
      ELSE
         PRINT *,'*** Error(s) in enquire routines ***'
      END IF

      END
