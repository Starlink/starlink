      SUBROUTINE TEST_CTOD(STATUS)
*+
*  Name:
*     TEST_CTOD

*  Purpose:
*     Test CHR_CTOD.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_CTOD(STATUS)

*  Description:
*     Test CHR_CTOD.
*     If any failure occurs, return STATUS = SAI__ERROR.
*     Otherwise, STATUS is unchanged.

*  Arguments:
*     STATUS = INTEGER (Returned)
*        The status of the tests.

*  Copyright:
*     Copyright (C) 1989, 1993, 1994 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     02-JUN-1995 (AJC)
*        Fix for Solaris bug
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Subprograms called:
*     CHR_CTOD

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
      CHARACTER*120 STRING
      DOUBLE PRECISION DP, DIFF  ! DP values
      CHARACTER*10 FIX
*.

*    Test CHR_CTOD

      DP = 0.0
      ISTAT = SAI__OK
*    Temporarily use FIX to overcome Solaris bug
      FIX = 'XXX'
      CALL CHR_CTOD (FIX, DP, ISTAT)
      IF (ISTAT .NE. SAI__ERROR) THEN
         PRINT *, 'CHR_CTOD FAILS - Error not detected'
         STATUS = SAI__ERROR
      ENDIF
      ISTAT = SAI__OK
      STRING = ' 3.333333333333'
      CALL CHR_CTOD (STRING, DP, ISTAT)
      DIFF = ABS(DP - 10.0D0/3.0D0)
      IF ((ISTAT .EQ. SAI__OK) .AND.
     :    (DIFF .LT. 5.0D-13)) THEN
         PRINT *, 'CHR_CTOD OK - Difference ',DIFF
      ELSE
         PRINT *, 'CHR_CTOD FAILS - '
         PRINT *, STRING,'read as',DP
         STATUS = SAI__ERROR
      ENDIF

      END
