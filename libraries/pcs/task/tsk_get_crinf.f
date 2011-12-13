      SUBROUTINE TASK_GET_CURRINFO ( SEQ, VALUE, DELAY, REQUEST,
     :  STATUS )
*+
*  Name:
*     TASK_GET_CURRINFO

*  Purpose:
*     Get details of current action

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL TASK_GET_CURRINFO ( SEQ, VALUE, DELAY, REQUEST, STATUS )

*  Description:
*     Gets details of the current action that may have been set as a result
*     of user calls in ACT. This simply involves copying information
*     from COMMON variables.

*  Arguments:
*     SEQ=INTEGER (returned)
*           the action sequence number
*     VALUE=CHARACTER*(*) (returned)
*           The action value string
*     DELAY=INTEGER (returned)
*           The delay before the next action entry in milliseconds. Depending
*           on the status returned from ACT, this may act as as a time-out
*           period. -1 means infinity.
*     REQUEST=INTEGER (returned)
*           The request returned by the application.
*     STATUS=INTEGER

*  Algorithm:
*     Copy information from COMMON.

*  Copyright:
*     Copyright (C) 1989-1993 Science & Engineering Research Council.
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
*     29-APR-1989 (AAOEPP::WFL):
*        Original
*     01-MAR-1990 (AAOEPP::WFL):
*        Add SEQ argument
*     23-APR-1991 (REVAD::BDK):
*        Rearrange INCLUDE files
*     06-MAY-1991 (REVAD::BDK):
*        Remove ADAMDEFNS
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

*  Arguments Returned:
      INTEGER SEQ           ! the action sequence number
      CHARACTER*(*) VALUE   ! the action value string
      INTEGER DELAY         ! the delay before the next action entry
      INTEGER REQUEST       ! The request returned by the application.

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'TASK_CMN'
*.
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Simply copy the values from COMMON.
*
      SEQ = CURACTSEQ
      VALUE = CURACTVALUE
      DELAY = CURACTDELAY
      REQUEST = CURACTREQUEST

      END
