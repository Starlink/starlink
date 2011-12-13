      SUBROUTINE TASK_PUT_MESSINFO ( PATH, CONTEXT, NAME, VALUE, MESSID,
     :  STATUS )
*+
*  Name:
*     TASK_PUT_MESSINFO

*  Purpose:
*     Set details of current action in subsidiary task

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL TASK_PUT_MESSINFO ( PATH, CONTEXT, NAME, VALUE, MESSID, STATUS)

*  Description:
*     Sets details of the action in a subsidiary task that has given rise to
*     this ACT entry so that user code can call routines to retrieve the
*     information. This simply involves copying information to COMMON variables.

*  Arguments:
*     PATH=INTEGER (given)
*           The path identifying the subsidiary task
*     CONTEXT=INTEGER (given)
*           The subsidiary action context (OBEY or CANCEL)
*     NAME=CHARACTER*(*) (given)
*           The subsidiary action name
*     VALUE=CHARACTER*(*) (given)
*           The subsidiary action value string
*     MESSID=INTEGER (given)
*           The message id identifying the action in the subsidiary task
*     STATUS=INTEGER
*           Not checked on entry since it is the message status. Not altered.

*  Algorithm:
*     Copy information to COMMON.

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
*     02-MAY-1989 (AAOEPP::WFL):
*        Alter interface to be more consistent
*     23-APR-1991 (REVAD::BDK):
*        Rearrange INCLUDE files
*     06-MAY-1991 (REVAD::BDK):
*        Remove ADAMDEFNS
*     24-MAY-1991 (REVAD::BDK):
*        Use CURRACTVALIN for VALUE
*     12-JUN-1991 (REVAD::BDK):
*        Use CURACTVALUE for VALUE
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
      INTEGER PATH          ! the path identifying the subsidiary task
      INTEGER CONTEXT       ! the subsidiary action context (OBEY or CANCEL)
      CHARACTER*(*) NAME    ! the subsidiary action name
      CHARACTER*(*) VALUE   ! the subsidiary action value string
      INTEGER MESSID        ! the message id id'ing the subsidiary action

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'TASK_CMN'
*.

*    Simply copy the values to COMMON.
*
      CURMESPATH = PATH
      CURMESCONTEXT = CONTEXT
      CURMESNAME = NAME
      CURACTVALUE = VALUE
      CURMESMESSID = MESSID
      CURMESSTATUS = STATUS

      END
