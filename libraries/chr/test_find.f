      SUBROUTINE TEST_FIND(STATUS)
*+
*  Name:
*     TEST_FIND

*  Purpose:
*     Test CHR_FIND.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_FIND(STATUS)

*  Description:
*     Test CHR_FIND.
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
*     02-MAR-1994 (ACC)
*        Second modularised version: broken further into one routine for
*        each of subroutine tested.  This subroutine created.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Subprograms called:
*     CHR_FIND

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
      INTEGER PTR1               ! String index
      INTEGER STRLEN             ! Length of STRING
      CHARACTER*120 STRING

*.

      STRLEN = LEN( STRING )

*    Test CHR_FIND

      STRING = '0123ABCDEFGHIJKABCLMNOP'

*    IPOSN invalid
      ISTAT = SAI__OK
      PTR1 = 150
      CALL CHR_FIND( STRING, 'ABC', .TRUE., PTR1 )
      IF ( PTR1 .NE. 150 ) THEN
         PRINT *, 'CHR_FIND invalid IPOSN FAILS'
         ISTAT = SAI__ERROR
      END IF

*    Substring not found
      PTR1 = 1
      CALL CHR_FIND( STRING, 'XYZ', .TRUE., PTR1 )
      IF ( PTR1 .NE. STRLEN + 1 ) THEN
         PRINT *, 'CHR_FIND Substring not found FAILS, PTR1 =',PTR1
         ISTAT = SAI__ERROR
      END IF

*    Substring found forward
      PTR1 = 1
      CALL CHR_FIND( STRING, 'ABC', .TRUE., PTR1 )
      IF ( PTR1 .NE. 5 ) THEN
         PRINT *, 'CHR_FIND Substring found forward FAILS, IPOSN =',PTR1
         ISTAT = SAI__ERROR
      END IF

*    Substring found backward
      PTR1 = 50
      CALL CHR_FIND( STRING, 'ABC', .FALSE., PTR1 )
      IF ( PTR1 .NE. 16 ) THEN
         PRINT *, 'CHR_FIND Substring found backward FAILS, IPOSN =',
     :             PTR1
         ISTAT = SAI__ERROR
      END IF

      IF ( ISTAT .EQ. SAI__OK ) THEN
         PRINT *, 'CHR_FIND OK'
      ELSE
         STATUS = ISTAT
      END IF

      END
