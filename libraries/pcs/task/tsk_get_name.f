      SUBROUTINE TASK_GET_NAME ( NAME, STATUS )
*+
*  Name:
*     TASK_GET_NAME

*  Purpose:
*     Get current action name

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL TASK_GET_NAME ( NAME, STATUS)

*  Description:
*     Gets current action name. This simply involves copying the name
*     from COMMON.

*  Arguments:
*     NAME=CHARACTER*(*) (returned)
*           The action name
*     STATUS=INTEGER

*  Algorithm:
*     Copy information from COMMON.

*  Copyright:
*     Copyright (C) 1989, 1991-1993 Science & Engineering Research
*     Council. All Rights Reserved.

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
*     29-APR-1989 (AAOEPP::WFL):
*        Original
*     23-APR-1991 (REVAD::BDK):
*        Rearrange INCLUDE files
*     06-MAY-1991 (REVAD::BDK):
*        Remove ADAMDEFNS, don't call SUBPAR
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

*  Arguments Returned:
      CHARACTER*(*) NAME  ! the action name

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'TASK_CMN'

*  Local Variables:
*.
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Simply copy the value from COMMON.
*
      NAME = CURACTNAME

      END
