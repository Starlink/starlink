      SUBROUTINE TASK_PUT_CURRINFO ( ACTPTR, CONTEXT, NAMECODE, ANAME,
     :  SEQ, VALUE, REQUEST, STATUS )
*+
*  Name:
*     TASK_PUT_CURRINFO

*  Purpose:
*     Set details of current action

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL TASK_PUT_CURRINFO ( ACTPTR, CONTEXT, NAMECODE, ANAME,
*     :  SEQ, VALUE, REQUEST, STATUS )

*  Description:
*     Sets details of the current action (excluding those that pertain to
*     actions in subsidiary tasks) so that user code can call routines to
*     retrieve the information. This simply involves copying information to
*     COMMON variables.

*  Arguments:
*     ACTPTR=INTEGER (given)
*           The action pointer
*     CONTEXT=INTEGER (given)
*           The action context (OBEY or CANCEL)
*     NAMECODE=INTEGER (given)
*           The action namecode
*     ANAME=CHARACTER*(*) (given)
*           The action name
*     SEQ=INTEGER (given)
*           The action sequence counter
*     VALUE=CHARACTER*(*) (given)
*           The action value string
*     REQUEST=INTEGER (given)
*           The stored application request
*     STATUS=INTEGER

*  Algorithm:
*     Copy information to COMMON. Set default of -1 (infinite) for delay
*     and blank for output value string.

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
*     06-MAY-1991: pass-in and store the action name. Remove ADAMDEFNS
*                  (REVAD::BDK)
*     12-JUN-1991 (REVAD::BDK):
*        Use CURACTVALUE
*     22-AUG-1991 (REVAD::BDK):
*        Add REQUEST
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
      INTEGER ACTPTR        ! the action pointer
      INTEGER CONTEXT       ! the action context (OBEY or CANCEL)
      INTEGER NAMECODE      ! the action namecode
      CHARACTER*(*) ANAME   ! the action name
      INTEGER SEQ           ! the action sequence counter
      CHARACTER*(*) VALUE   ! the action value string
      INTEGER REQUEST       ! the stored application request

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'TASK_CMN'
*.
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Simply copy the values to COMMON.
*
      CURACTPTR = ACTPTR
      CURACTCONTEXT = CONTEXT
      CURACTNAMECODE = NAMECODE
      CURACTNAME = ANAME
      CURACTSEQ = SEQ
      CURACTVALUE = VALUE
      CURACTREQUEST = REQUEST
      CURACTDELAY = -1

      END
