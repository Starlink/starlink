      SUBROUTINE TEST_WILD(STATUS)
*+
*  Name:
*     TEST_WILD

*  Purpose:
*     Test CHR_WILD.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_WILD(STATUS)

*  Description:
*     Test CHR_WILD.
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
*     02-MAR-1994 (ACC)
*        Second modularised version: broken further into one routine for
*        each of subroutine tested.  This subroutine created.
*     30-MAY-1995 (AJC):
*        Correct use of LSTAT
*        Remove JSTAT
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Subprograms called:
*     CHR_WILD

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

*  External References:
      LOGICAL CHR_WILD

*  Local Variables:
      INTEGER ISTAT              ! Local status
      CHARACTER*120 STRING
      CHARACTER*30 WILDS
      CHARACTER*30 MATCH
      LOGICAL LSTAT

*.

*    Test CHR_WILD

      ISTAT = SAI__OK
      LSTAT = .TRUE.
      STRING = 'notredame'
      WILDS = 'n*%'
      MATCH = ' '
      LSTAT = CHR_WILD( STRING(1:9), WILDS(1:3), MATCH )
      IF ( .NOT. LSTAT .OR.
     :     MATCH(1:9) .NE. ' *******%' ) THEN
         PRINT *, 'CHR_WILD fails test a, STRING =',STRING(1:15),
     :             ' WILDS =', WILDS(1:15),' MATCH =',MATCH(1:15)
         ISTAT = SAI__ERROR
      ENDIF

      MATCH = ' '
      WILDS = 'n*e'
      LSTAT = CHR_WILD( STRING(1:9), WILDS(1:3), MATCH )
      IF ( .NOT. LSTAT .OR.
     :     MATCH(1:9) .NE. ' ******* ' ) THEN
         PRINT *, 'CHR_WILD fails test b, STRING =',STRING(1:15),
     :             ' WILDS =', WILDS(1:15),' MATCH =',MATCH(1:15)
         ISTAT = SAI__ERROR
      ENDIF

      MATCH = ' '
      WILDS = 'not*%%e'
      LSTAT = CHR_WILD( STRING(1:9), WILDS(1:7), MATCH )
      IF ( .NOT. LSTAT .OR.
     :     MATCH(1:9) .NE. '   ***%% ' ) THEN
         PRINT *, 'CHR_WILD fails test c, STRING =',STRING(1:15),
     :             ' WILDS =', WILDS(1:15),' MATCH =',MATCH(1:15)
         ISTAT = SAI__ERROR
      ENDIF

      MATCH = ' '
      WILDS = 'n**********e'
      LSTAT = CHR_WILD( STRING(1:9), WILDS(1:12), MATCH )
      IF ( .NOT. LSTAT .OR.
     :     MATCH(1:9) .NE. ' ******* ' ) THEN
         PRINT *, 'CHR_WILD fails test d, STRING =',STRING(1:15),
     :             ' WILDS =', WILDS(1:15),' MATCH =',MATCH(1:15)
         ISTAT = SAI__ERROR
      ENDIF

      MATCH = ' '
      WILDS = 'n*%%%e**%e'
      LSTAT = CHR_WILD( STRING(1:9), WILDS(1:10), MATCH )
      IF ( .NOT. LSTAT .OR.
     :     MATCH(1:9) .NE. ' %%% **% ' ) THEN
         PRINT *, 'CHR_WILD fails test e, STRING =',STRING(1:15),
     :             ' WILDS =', WILDS(1:15),' MATCH =',MATCH(1:15)
         ISTAT = SAI__ERROR
      ENDIF

      MATCH = ' '
      WILDS = '*'
      LSTAT = CHR_WILD( STRING(1:9), WILDS(1:1), MATCH )
      IF ( .NOT. LSTAT .OR.
     :     MATCH(1:9) .NE. '*********' ) THEN
         PRINT *, 'CHR_WILD fails test f, STRING =',STRING(1:15),
     :             ' WILDS =', WILDS(1:15),' MATCH =',MATCH(1:15)
         ISTAT = SAI__ERROR
      ENDIF

      MATCH = ' '
      WILDS = '*notredame'
      LSTAT = CHR_WILD( STRING(1:9), WILDS(1:10), MATCH )
      IF ( .NOT. LSTAT .OR.
     :     MATCH(1:9) .NE. '         ' ) THEN
         PRINT *, 'CHR_WILD fails test g, STRING =',STRING(1:15),
     :             ' WILDS =', WILDS(1:15),' MATCH =',MATCH(1:15)
         ISTAT = SAI__ERROR
      ENDIF

      MATCH = ' '
      WILDS = 'notredame*'
      LSTAT = CHR_WILD( STRING(1:9), WILDS(1:10), MATCH )
      IF ( .NOT. LSTAT .OR.
     :     MATCH(1:9) .NE. '         ' ) THEN
         PRINT *, 'CHR_WILD fails test h, STRING =',STRING(1:15),
     :             ' WILDS =', WILDS(1:15),' MATCH =',MATCH(1:15)
         ISTAT = SAI__ERROR
      ENDIF

      MATCH = ' '
      WILDS = 'n*'
      LSTAT = CHR_WILD( STRING(1:9), WILDS(1:2), MATCH )
      IF ( .NOT. LSTAT .OR.
     :     MATCH(1:9) .NE. ' ********' ) THEN
         PRINT *, 'CHR_WILD fails test i, STRING =',STRING(1:15),
     :             ' WILDS =', WILDS(1:15),' MATCH =',MATCH(1:15)
         ISTAT = SAI__ERROR
      ENDIF

      MATCH = ' '
      WILDS = 'n%*'
      LSTAT = CHR_WILD( STRING(1:9), WILDS(1:3), MATCH )
      IF ( .NOT. LSTAT .OR.
     :     MATCH(1:9) .NE. ' %*******' ) THEN
         PRINT *, 'CHR_WILD fails test j, STRING =',STRING(1:15),
     :             ' WILDS =', WILDS(1:15),' MATCH =',MATCH(1:15)
         ISTAT = SAI__ERROR
      ENDIF

      MATCH = ' '
      WILDS = 'n*e*e'
      LSTAT = CHR_WILD( STRING(1:9), WILDS(1:5), MATCH )
      IF ( .NOT. LSTAT .OR.
     :     MATCH(1:9) .NE. ' *** *** ' ) THEN
         PRINT *, 'CHR_WILD fails test k, STRING =',STRING(1:15),
     :             ' WILDS =', WILDS(1:15),' MATCH =',MATCH(1:15)
         ISTAT = SAI__ERROR
      ENDIF

      MATCH = ' '
      WILDS = 'n%%%%%%%%%'
      LSTAT = CHR_WILD( STRING(1:9), WILDS(1:10), MATCH )
      IF ( LSTAT ) THEN
         PRINT *, 'CHR_WILD fails test l, STRING =',STRING(1:15),
     :             ' WILDS =', WILDS(1:15),' MATCH =',MATCH(1:15)
         ISTAT = SAI__ERROR
      ENDIF

      MATCH = ' '
      WILDS = 'n%%%%%%%%'
      LSTAT = CHR_WILD( STRING(1:9), WILDS(1:9), MATCH )
      IF ( .NOT. LSTAT .OR.
     :     MATCH(1:9) .NE. ' %%%%%%%%' ) THEN
         PRINT *, 'CHR_WILD fails test m, STRING =',STRING(1:15),
     :             ' WILDS =', WILDS(1:15),' MATCH =',MATCH(1:15)
         ISTAT = SAI__ERROR
      ENDIF

      MATCH = ' '
      WILDS = 'n**********e*e'
      LSTAT = CHR_WILD( STRING(1:9), WILDS(1:14), MATCH )
      IF ( .NOT. LSTAT .OR.
     :     MATCH(1:9) .NE. ' *** *** ' ) THEN
         PRINT *, 'CHR_WILD fails test n, STRING =',STRING(1:15),
     :             ' WILDS =', WILDS(1:15),' MATCH =',MATCH(1:15)
         ISTAT = SAI__ERROR
      ENDIF

      MATCH = ' '
      WILDS = 'n*e*'
      LSTAT = CHR_WILD( STRING(1:9), WILDS(1:4), MATCH )
      IF ( .NOT. LSTAT .OR.
     :     MATCH(1:9) .NE. ' *** ****' ) THEN
         PRINT *, 'CHR_WILD fails test o, STRING =',STRING(1:15),
     :             ' WILDS =', WILDS(1:15),' MATCH =',MATCH(1:15)
         ISTAT = SAI__ERROR
      ENDIF

      MATCH = ' '
      WILDS = 'n'
      LSTAT = CHR_WILD( STRING(1:9), WILDS(1:1), MATCH )
      IF ( LSTAT ) THEN
         PRINT *, 'CHR_WILD fails test p'
         PRINT *, 'STRING =',STRING(1:15),
     :             ' WILDS =', WILDS(1:15),' MATCH =',MATCH(1:15)
         PRINT *, 'LSTAT =',LSTAT
         ISTAT = SAI__ERROR
      ENDIF

      MATCH = ' '
      WILDS = 'n*m'
      LSTAT = CHR_WILD( STRING(1:9), WILDS(1:3), MATCH )
      IF ( LSTAT ) THEN
         PRINT *, 'CHR_WILD fails test q, STRING =',STRING(1:15),
     :             ' WILDS =', WILDS(1:15),' MATCH =',MATCH(1:15)
         ISTAT = SAI__ERROR
      ENDIF

      STRING = 'graphics'
      MATCH = ' '
      WILDS = 'g*'
      LSTAT = CHR_WILD( STRING(1:8), WILDS(1:2), MATCH )
      IF ( .NOT. LSTAT .OR.
     :     MATCH(1:8) .NE. ' *******' ) THEN
         PRINT *, 'CHR_WILD fails test r, STRING =',STRING(1:15),
     :             ' WILDS =', WILDS(1:15),' MATCH =',MATCH(1:15)
         ISTAT = SAI__ERROR
      ENDIF

      STRING = 'removing_stars'
      MATCH = ' '
      WILDS = 'g*'
      LSTAT = CHR_WILD( STRING(1:14), WILDS(1:2), MATCH )
      IF ( LSTAT ) THEN
         PRINT *, 'CHR_WILD fails test s, STRING =',STRING(1:15),
     :             ' WILDS =', WILDS(1:15),' MATCH =',MATCH(1:15)
         PRINT *,'LSTAT =',LSTAT
         ISTAT = SAI__ERROR
      ENDIF

      STRING = 'antidisestablishmentarianism'
      MATCH = ' '
      WILDS = '*t%%ian*'
      LSTAT = CHR_WILD( STRING(1:28), WILDS(1:8), MATCH )
      IF ( .NOT. LSTAT .OR.
     :    MATCH(1:28) .NE. '******************* %%   ***' ) THEN
         PRINT *, 'CHR_WILD fails test t, STRING =',STRING(1:15),
     :             ' WILDS =', WILDS(1:15),' MATCH =',MATCH(1:15)
         ISTAT = SAI__ERROR
      ENDIF

      IF ( ISTAT .EQ. SAI__OK ) THEN
         PRINT *, 'CHR_WILD OK'
      ELSE
         STATUS = SAI__ERROR
      ENDIF

      END
