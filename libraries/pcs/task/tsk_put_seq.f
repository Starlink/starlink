      SUBROUTINE TASK_PUT_SEQ ( SEQ, STATUS )
*+
*  Name:
*     TASK_PUT_SEQ

*  Purpose:
*     Set current action sequence counter

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL TASK_PUT_SEQ ( SEQ, STATUS )

*  Description:
*     Sets the current action sequence counter.

*  Arguments:
*     SEQ=INTEGER (given)
*           The current action sequence counter
*     STATUS=INTEGER

*  Algorithm:
*     Copy information to COMMON.

*  Copyright:
*     Copyright (C) 1990-1993 Science & Engineering Research Council.
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
*     W.F.Lupton (AAOEPP::WFL)
*     {enter_new_authors_here}

*  History:
*     01-MAR-1990 (AAOEPP::WFL):
*        Original
*     23-APR-1991 (REVAD::BDK):
*        Rearrange INCLUDE files
*     06-MAY-1991 (REVAD::BDK):
*        Remove ADAMDEFNS
*     04-OCT-1992 (RLVAD::AJC):
*        Add PAR_PAR for porting
*     24-AUG-1993 (RLVAD::AJC):
*        Use SUBPAR_SYS not PAR_PAR
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_SYS'
      INCLUDE 'MESSYS_LEN'
      INCLUDE 'TASK_PAR'

*  Arguments Given:
      INTEGER SEQ     ! the current action sequence counter

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'TASK_CMN'
*.
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Simply copy the sequence counter to COMMON.
*
      CURACTSEQ = SEQ

      END
