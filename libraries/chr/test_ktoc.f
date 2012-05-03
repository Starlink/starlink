      SUBROUTINE TEST_KTOC(STATUS)
*+
*  Name:
*     TEST_KTOC

*  Purpose:
*     Test CHR_KTOC.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_KTOC(STATUS)

*  Description:
*     Test CHR_KTOC.
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
*        Copy from TEST_ITOC
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Subprograms called:
*     CHR_KTOC

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

*  Local Constants:
      INTEGER * 8 INTIN1
      PARAMETER ( INTIN1 = 9223372036854775800 )
      INTEGER * 8 INTIN2
      PARAMETER ( INTIN2 = -9223372036854775801 )

*  Local Variables:
      INTEGER PTR1,              ! String indexes
     :        ISTAT              ! Local status
      CHARACTER*120 STRING

*.

*    Test CHR_KTOC -- Integer*8 to decimal character

      ISTAT = SAI__OK
      STRING = ' '
      PTR1 = 0
      CALL CHR_KTOC (INTIN1, STRING, PTR1)
      IF (STRING(1:PTR1) .NE. '9223372036854775800') THEN
         PRINT *, 'CHR_KTOC FAILS- STRING is:',STRING
         ISTAT = SAI__ERROR
      ENDIF

      STRING = ' '
      PTR1 = 0
      CALL CHR_KTOC (INTIN2, STRING, PTR1)
      IF (STRING(1:PTR1) .NE. '-9223372036854775801') THEN
         PRINT *, 'CHR_KTOC FAILS- STRING is:',STRING
         ISTAT = SAI__ERROR
      ENDIF

      IF (ISTAT .EQ. SAI__OK) THEN
         PRINT *, 'CHR_KTOC OK'
      ELSE
         STATUS = SAI__ERROR
      ENDIF

      END
