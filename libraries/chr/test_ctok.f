      SUBROUTINE TEST_CTOK(STATUS)
*+
*  Name:
*     TEST_CTOK

*  Purpose:
*     Test CHR_CTOK.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_CTOI(STATUS)

*  Description:
*     Test CHR_CTOI.
*     If any failure occurs, return STATUS = SAI__ERROR.
*     Otherwise, STATUS is unchanged.

*  Arguments:
*     STATUS = INTEGER (Returned)
*        The status of the tests.

*  Copyright:
*     Copyright (C) 2012 Science & Technology Facilities Council.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
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
*     2012-05-03 (TIMJ):
*        Copy from TEST_CTOI
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Subprograms called:
*     CHR_CTOK

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

*  Local constants
      INTEGER *8 TESTINT
      PARAMETER ( TESTINT = -9223372036854775800 )

*  Local Variables:
      INTEGER ISTAT              ! Local status
      INTEGER*8 I                ! INTEGER value
      CHARACTER*120 STRING

*.

*    Test CHR_CTOI

      I = 0
      ISTAT = SAI__OK
      CALL CHR_CTOK ('XXX', I, ISTAT)
      IF (ISTAT .NE. SAI__ERROR) THEN
         PRINT *, 'CHR_CTOK FAILS - Error not detected'
         STATUS = SAI__ERROR
      ENDIF
      ISTAT = SAI__OK
      STRING = '3 -9223372036854775800'
      CALL CHR_CTOK (STRING(3:23), I, ISTAT)
      IF ((ISTAT .EQ. SAI__OK) .AND. (I .EQ. TESTINT)) THEN
         PRINT *, 'CHR_CTOK OK'
      ELSE
         PRINT *, 'CHR_CTOK FAILS - '
         PRINT *, STRING,'read as',I
         STATUS = SAI__ERROR
      ENDIF

      END
