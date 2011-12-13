      SUBROUTINE TASK_ASKPARAM ( PATH, VALUE, MESSID, STATUS )
*+
*  Name:
*     TASK_ASKPARAM

*  Purpose:
*     Ask user for parameter value

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL TASK_ASKPARAM ( PATH, VALUE, MESSID, STATUS )

*  Description:
*     Given parameter info in "value", ask user (by prompting) for
*     a value for the parameter.

*  Arguments:
*     PATH=INTEGER (Given)
*           path to requesting task
*     VALUE=CHARACTER*(*) (Given)
*           message value string
*     MESSID=INTEGER (Given)
*           ID for reply message
*     STATUS=INTEGER

*  Algorithm:
*     The value string contains substrings separated by nulls. The
*     substrings are the parameter name, its prompt string, its default
*     value, its help information, and an error message if an
*     earlier attempt to get the parameter has failed. This information
*     is used by ASKPARAM to prompt the user.
*     The value obtained is then sent to the task which requested the
*     parameter.

*  Copyright:
*     Copyright (C) 1984-1985, 1987, 1990-1993 Science & Engineering
*     Research Council. Copyright (C) 2001 Central Laboratory of the
*     Research Councils. All Rights Reserved.

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
*     John Cooke (REVA::ADAM) 7Nov84
*     {enter_new_authors_here}

*  History:
*     07-NOV-1984 (REVAD::JAC):
*        Original version
*     29-MAR-1985 (REVAD::BDK):
*        UFACE version
*     18-OCT-1985 (REVAD::BDK):
*        Version for cli running as a task
*     23-JUL-1987 (REVAD::BDK):
*        Make PARHELP 132 long
*     05-NOV-1987 (REVAD::BDK):
*        Version for TASK
*     05-DEC-1990: call SUBPAR_SPLITVAL to split revised message
*                 value structure (RLVAD::AJC)
*     06-MAY-1991 (REVAD::BDK):
*        Revise include files
*     25-NOV-1991 (REVAD::BDK):
*        Use ADAM_ACKNOW
*     04-OCT-1992: add PAR_PAR for porting
*                 replace STR$TRIM with CHR_LEN (RLVAD::AJC)
*     24-AUG-1993: Use SUBPAR_SYS not PAR_PAR,
*                  SUBPAR__NAMELEN not PAR__SZNAM
*                  and SUBPAR_PARERR not PAR_ERR (RLVAD::AJC)
*     11-JUN-2001: Use AMS (FAMS) _REPLY not ADAM_ACKNOW
*                 Use AMS (FAMS) _PLOOKUP not MESSYS_PLOOKUP
*                 Change MSG_VAL_LEN to MESSYS__VAL_LEN (AJC)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_SYS'
      INCLUDE 'SUBPAR_PARERR'
      INCLUDE 'MESSYS_PAR'
      INCLUDE 'MESSYS_ERR'
      INCLUDE 'MESSYS_LEN'

*  Arguments Given:
      INTEGER PATH                 ! path to requesting task
      CHARACTER VALUE*(*)          ! message value string
      INTEGER MESSID               ! ID for reply message

*  Status:
      INTEGER STATUS

*    External Routines :
      INTEGER CHR_LEN              ! used length of string
      EXTERNAL CHR_LEN

*  Local Constants:
      INTEGER NOCONTEXT
      PARAMETER ( NOCONTEXT = 0 )
      CHARACTER NONAME
      PARAMETER ( NONAME = ' ' )

*  Local Variables:
      CHARACTER*(MESSYS__TNAME) TASKNAME ! name of task which requested
                                         ! value
      INTEGER TNAMELEN                   ! length of TASKNAME
      CHARACTER*(SUBPAR__NAMELEN) PARAM  ! parameter name
      INTEGER PARLEN                     ! length of parameter name
      CHARACTER*80 PROMPT                ! parameter prompt string
      INTEGER PRMPLEN                    ! length of parameter prompt
                                         ! string
      CHARACTER*80 DEFAULT               ! default value string
      INTEGER DEFLEN                     ! length of default value
                                         ! string
      CHARACTER*132 PARHELP              ! one-line help specifier
      INTEGER HELPLEN                    ! length of one-line help
                                         ! specifier
      CHARACTER*132 HLPKEY               ! full-help specifier
      INTEGER HKYLEN                     ! length of full-help specifier
      CHARACTER*100 ERRMESS              ! error message
      INTEGER ERRLEN                     ! length of error message
      CHARACTER*(MESSYS__VAL_LEN) INVAL  ! value from user
      INTEGER MESSTATUS                  ! forwarded message status
*.

      IF ( STATUS .NE. SAI__OK ) RETURN
*
*   Find name of task which sent the request.
*
      CALL FAMS_PLOOKUP ( PATH, TASKNAME, STATUS )
      TNAMELEN = CHR_LEN( TASKNAME )
*
*   Extract parameter info components.
*
      CALL SUBPAR_SPLITVAL( VALUE, PARAM, PARLEN, PROMPT, PRMPLEN,
     :  DEFAULT, DEFLEN, PARHELP, HELPLEN, HLPKEY, HKYLEN, ERRMESS,
     :  ERRLEN, STATUS)

*
*   Send the prompt to the user interface.
*
      CALL SUBPAR_REQUEST ( PARAM(1:PARLEN), PROMPT(1:PRMPLEN),
     :  DEFAULT(1:DEFLEN), PARHELP(1:HELPLEN), HLPKEY(1:HKYLEN),
     :  ERRMESS(1:ERRLEN), INVAL, STATUS )
*
*   Trap non-ok conditions. These have to be translated into things
*   which the parameter system at the receiving task will interpret
*   correctly. In particular, it will convert timeout into PAR__NOUSR.
*
      MESSTATUS = MESSYS__PARAMREP
      IF ( STATUS .EQ. PAR__NULL ) THEN
         INVAL = '!'
      ELSE IF ( STATUS .EQ. PAR__ABORT ) THEN
         INVAL = '!!'
      ELSE IF ( STATUS .EQ. PAR__NOUSR ) THEN
         MESSTATUS = MESSYS__TIMEOUT
      ENDIF

      STATUS = SAI__OK

*
*   Have got value, send it back to the requesting task.
*
      CALL FAMS_REPLY( PATH, MESSID, MESSYS__MESSAGE, MESSTATUS,
     :  NOCONTEXT, NONAME, MESSYS__VAL_LEN, INVAL, STATUS )

      END
