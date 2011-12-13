      SUBROUTINE TEST_ITOH(STATUS)
*+
*  Name:
*     TEST_ITOH

*  Purpose:
*     Test CHR_ITOH.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_ITOH(STATUS)

*  Description:
*     Test CHR_ITOH.
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
*     CHR_ITOH

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
      INTEGER ISTAT,JSTAT        ! Local status
      CHARACTER*120 STRING

*.

*    Test CHR_ITOH -- Integer to Hex character string

      ISTAT = SAI__OK
      JSTAT = SAI__OK
      STRING = ' '
      CALL CHR_ITOH (86, STRING, ISTAT)
      IF (STRING(1:2) .NE. '56' .OR. ISTAT .NE. SAI__OK) THEN
         PRINT *, 'CHR_ITOH FAILS- STRING is:',STRING
         JSTAT = SAI__ERROR
      ENDIF

      STRING = ' '
      CALL CHR_ITOH (-1, STRING, ISTAT)
      IF (STRING(1:8) .NE. 'FFFFFFFF' .OR. ISTAT .NE. SAI__OK) THEN
         PRINT *, 'CHR_ITOH FAILS- STRING is:',STRING
         JSTAT = SAI__ERROR
      ENDIF

      IF (JSTAT .EQ. SAI__OK) THEN
         PRINT *, 'CHR_ITOH OK'
      ELSE
         STATUS = SAI__ERROR
      ENDIF

      END
