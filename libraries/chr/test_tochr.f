      SUBROUTINE TEST_TOCHR(STATUS)
*+
*  Name:
*     TEST_TOCHR

*  Purpose:
*     Test CHR_TOCHR.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_TOCHR(STATUS)

*  Description:
*     Test CHR_TOCHR.
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
*     CHR_TOCHR

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

*    Test CHR_TOCHR

      ISTAT = SAI__OK
      STRING = 'ABCDEFGABCDEFG'

*    Chars don't exist, forward
      PTR1 = 1
      CALL CHR_TOCHR( 'XYZ', STRING, .TRUE., PTR1 )
      IF ( PTR1 .NE. STRLEN + 1 ) THEN
         PRINT *, 'CHR_TOCHR chars don''t exist forward FAILS,',
     :             ' IPOSN =', PTR1
         ISTAT = SAI__ERROR
      END IF

*    Chars don't exist, backward
      PTR1 = 30
      CALL CHR_TOCHR( 'XYZ', STRING, .FALSE., PTR1 )
      IF ( PTR1 .NE. 0 ) THEN
         PRINT *, 'CHR_TOCHR chars don''t exist backward FAILS,',
     :            ' IPOSN =',PTR1
         ISTAT = SAI__ERROR
      END IF

*    Initial position invalid
      PTR1 = 150
      CALL CHR_TOCHR( 'CG', STRING, .TRUE., PTR1 )
      IF ( PTR1 .NE. 150 ) THEN
         PRINT *, 'CHR_TOCHR initial position invalid FAILS, IPOSN =',
     :             PTR1
         ISTAT = SAI__ERROR
      END IF

*    Valid inputs, forward
      PTR1 = 1
      CALL CHR_TOCHR( 'CG', STRING, .TRUE., PTR1 )
      IF ( PTR1 .NE. 3 ) THEN
         PRINT *, 'CHR_TOCHR valid inputs forward FAILS, IPOSN =',
     :             PTR1
         ISTAT = SAI__ERROR
      END IF

*    Valid inputs, backward
      PTR1 = 20
      CALL CHR_TOCHR( 'CG', STRING, .FALSE., PTR1 )
      IF ( PTR1 .NE. 14 ) THEN
         PRINT *, 'CHR_TOCHR valid inputs backward FAILS, IPOSN =',
     :             PTR1
         ISTAT = SAI__ERROR
      END IF

      IF ( ISTAT .EQ. SAI__OK ) THEN
         PRINT *, 'CHR_TOCHR OK'
      ELSE
         STATUS = SAI__ERROR
      ENDIF

      END
