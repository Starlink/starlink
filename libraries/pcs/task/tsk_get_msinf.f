      SUBROUTINE TASK_GET_MESSINFO ( PATH, CONTEXT, NAME, VALUE, MESSID,
     :  EVENT, STATUS )
*+
*  Name:
*     TASK_GET_MESSINFO

*  Purpose:
*     Get details of current action in subsidiary task

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL TASK_GET_MESSINFO ( PATH, CONTEXT, NAME, VALUE, MESSID, EVENT,
*     STATUS)

*  Description:
*     Gets details of the action in a subsidiary task that has given rise to
*     this ACT entry. This simply involves copying information from COMMON
*     variables.

*  Arguments:
*     PATH=INTEGER (returned)
*           The path identifying the subsidiary task
*     CONTEXT=INTEGER (returned)
*           The subsidiary action context (OBEY or CANCEL)
*     NAME=INTEGER (returned)
*           The subsidiary action name
*     VALUE=CHARACTER*(*) (returned)
*           The subsidiary action value string
*     MESSID=INTEGER (returned)
*           The message id identifying the action in the subsidiary task
*     EVENT=INTEGER (returned)
*           The subsidiary action message status
*     STATUS=INTEGER
*           Normal inherited status

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
*     17-SEP-1991 (RLVAD::AJC):
*        Separate EVENT and STATUS
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
      INTEGER PATH          ! the path identifying the subsidiary task
      INTEGER CONTEXT       ! the subsidiary action context (OBEY or CANCEL)
      CHARACTER*(*) NAME    ! the subsidiary action name
      CHARACTER*(*) VALUE   ! the subsidiary action value string
      INTEGER MESSID        ! the message id id'ing the subsidiary action
      INTEGER EVENT         ! the subsidiary action message status

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'TASK_CMN'
*.
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Simply copy the values from COMMON.
*
      PATH = CURMESPATH
      CONTEXT = CURMESCONTEXT
      NAME = CURMESNAME
      VALUE = CURACTVALUE
      MESSID = CURMESMESSID
      EVENT = CURMESSTATUS

      END
