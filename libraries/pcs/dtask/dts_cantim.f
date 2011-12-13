      SUBROUTINE DTASK_CANTIM ( ACTPTR, STATUS )
*+
*  Name:
*     DTASK_CANTIM

*  Purpose:
*     Unix version: cancel the timer for an action

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL DTASK_CANTIM ( ACTPTR, STATUS )

*  Description:
*     Cancel the timer for the specified action if there is one.

*  Arguments:
*     ACTPTR=INTEGER (given)
*           index to DTASK common blocks for the action
*     STATUS=INTEGER

*  Algorithm:
*     Look-up the timer id for this action. If it is non-zero it has
*     been used to declare a timer, so cancel the timer (even though it
*     may have gone off), set the timer id to zero and increment the
*     action counter so that the next timer id generated will be
*     different.

*  Copyright:
*     Copyright (C) 1991-1994 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     03-MAY-1991 (REVAD::BDK):
*        Original
*     13-OCT-1992 (RLVAD::AJC):
*        Add INCLUDE 'PAR_PAR'
*     23-AUG-1993 (RLVAD::AJC):
*        Replace PAR_PAR with SUBPAR_SYS
*     29-SEP-1993 (RLVAD::BKM):
*        Unix version
*     27-JUN-1994 (RLVAD::AJC):
*        Version for Unix V2 message system
*     19-AUG-1994 (RLVAD::BKM):
*        Ignore Unix cancel timer status
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_SYS'
      INCLUDE 'DTASK_SYS'

*  Arguments Given:
      INTEGER ACTPTR   ! index to DTASK common blocks for the action

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'DTASK_CMN'

*    Local variables
      INTEGER LSTAT
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( ACTTIM(ACTPTR) .NE. 0 ) THEN
*
*      A timer has been declared for this action. It may already have gone off.
*      This is ignored on VMS but the Unix cancel timer routine can return
*      ATIMER__NOTFOUND. Just use a local status at present.
*
         LSTAT = SAI__OK
         CALL FATIMER_CANTIM( ACTTIM(ACTPTR), LSTAT )
         ACTTIM(ACTPTR) = 0
         ACTCOUNT(ACTPTR) = ACTCOUNT(ACTPTR) + 1
         IF ( ACTCOUNT(ACTPTR) .GT. DTASK__MAXACTTOT ) THEN
            ACTCOUNT(ACTPTR) = 1
         ENDIF

      ENDIF

      END
