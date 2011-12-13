      SUBROUTINE TEST_SEARCH(STATUS)
*+
*  Name:
*     TEST_SEARCH

*  Purpose:
*     Test the string search routines.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_SEARCH(STATUS)

*  Description:
*     Test each of the string search routines listed in Appendix A.3
*     of SUN/40.3.
*     If any failure occurs, return STATUS = SAI__ERROR.
*     Otherwise, STATUS is unchanged.

*  Arguments:
*     STATUS = INTEGER (Returned)
*        The status of the tests.

*  Copyright:
*     Copyright (C) 1993, 1994 Science & Engineering Research Council.
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     14-SEP-1993 (ACC)
*        Original version.
*     02-MAR-1994 (ACC)
*        Broke into separate routines for each routine tested.
*     23-DEC-2005 (TIMJ):
*        Call TEST_FPARX
*     27-DEC-2005 (TIMJ):
*        Call TEST_LASTO
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Subprograms called:
*     TEST_DELIM, TEST_FANDL, TEST_FIND, TEST_FIWES, TEST_INDEX,
*     TEST_SKCHR, TEST_TOCHR, TEST_FPARX, TEST_LASTO
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
      PRINT *,'*** Test search string routines ***'

      STATUS = SAI__OK

*    Test CHR_DELIM

      ISTAT = SAI__OK
      CALL TEST_DELIM(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_FANDL

      ISTAT = SAI__OK
      CALL TEST_FANDL(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_FIND

      ISTAT = SAI__OK
      CALL TEST_FIND(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_FIWES

      ISTAT = SAI__OK
      CALL TEST_FIWES(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_INDEX

      ISTAT = SAI__OK
      CALL TEST_INDEX(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_SKCHR

      ISTAT = SAI__OK
      CALL TEST_SKCHR(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_TOCHR

      ISTAT = SAI__OK
      CALL TEST_TOCHR(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_FPARX

      ISTAT = SAI__OK
      CALL TEST_FPARX(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Test CHR_LASTO

      ISTAT = SAI__OK
      CALL TEST_LASTO(ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
      END IF

*    Write summary message

      IF (STATUS .EQ. SAI__OK) THEN
         PRINT *,'*** All search string routines OK ***'
      ELSE
         PRINT *,'*** Error(s) in search string routines ***'
      END IF

      END
