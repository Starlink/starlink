      SUBROUTINE TEST_PUTD(STATUS)
*+
*  Name:
*     TEST_PUTD

*  Purpose:
*     Test CHR_PUTD.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_PUTD(STATUS)

*  Description:
*     Test CHR_PUTD.
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
*     CHR_PUTD

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
      INTEGER PTR1               ! String indexes
      CHARACTER*120 STRING
      DOUBLE PRECISION DP        ! DP values

*.

*    Test CHR_PUTD -- Double precision to character at given position

      STRING = ' '
      PTR1 = 1
      DP = 10.0D0/3.0D0
      CALL CHR_PUTD (DP, STRING, PTR1)

      IF (PTR1 .GE. 15 .AND.
     :   STRING(1:15) .EQ. ' 3.333333333333') THEN
         PRINT *, 'CHR_PUTD OK'
      ELSE
         PRINT *, 'CHR_PUTD FAILS- STRING is:',STRING
         STATUS = SAI__ERROR
      ENDIF

      END
