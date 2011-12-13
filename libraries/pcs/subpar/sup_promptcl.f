      SUBROUTINE SUBPAR_PROMPTCL ( PARAM, PROMPT, DFAULT,
     :  HLPTXT, HLPKEY, ERRMES, PARVAL, STATUS )
*+
*  Name:
*     SUBPAR_PROMPTCL

*  Purpose:
*     request parameter value from user interface.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_PROMPTCL ( PARAM, PROMPT, DFAULT,

*  Description:
*     Locates the calling task, then constructs the value string
*     for a message with the parameters given, sends a message asking
*     for the parameter value, and waits for the reply.

*  Arguments:
*     PARAM=CHARACTER*(*) (given)
*        name of parameter
*     PROMPT=CHARACTER*(*) (given)
*        prompt string
*     DFAULT=CHARACTER*(*) (given)
*        default value
*     HLPTXT=CHARACTER*(*) (given)
*        one-line help text
*     HLPKEY=CHARACTER*(*) (given)
*        full-help specifier
*     ERRMES=CHARACTER*(*) (given)
*        error message
*     PARVAL=CHARACTER*(*) (returned)
*        value obtained
*     STATUS=INTEGER

*  Algorithm:
*     Get the path and messid to the task which issued the run command.
*     Concatenate the various pieces of prompt/help/default information
*     into a character string, the individual components separated by CHAR(0).
*     Send the result to the controlling task using ADAM_ACKNOW so that
*     the existing message id is used. Accept the reply and
*     return it as a text string. The replies ! and !! are identified
*     and the STATUS return set to PAR__NULL and PAR__ABORT
*     respectively. If there is no path (RUNPATH=0) or if no reply appears
*     before the TIMEOUT period, then STATUS PAR__NOUSR is returned.
*     (Currently TIMEOUT is infinite).

*  Copyright:
*     Copyright (C) 1984, 1985, 1988, 1990, 1991, 1992, 1993, 1994 Science & Engineering Research Council.
*     Copyright (C) 1995, 1996, 2001 Central Laboratory of the Research Councils.
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
*     John Cooke (REVA::ADAM) 7Nov84
*     JAC: J A Cooke (ROE)
*     BDK: B D Kelly (ROE)
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     07-NOV-1984 (JAC):
*        Original version
*     09-NOV-198 (BDK):
*        moved from DTASK_ to SUBPAR_
*     13-FEB-1985 (BDK):
*        ensure none of the substrings sent are null
*     22-MAR-1985 (BDK):
*        use GETREPLY to ensure answer from correct task
*     16-APR-1985 (BDK):
*        use stored RUNID with MESSYS_REPLY call so that
*        controlling task will get message even if IT is using
*        GETREPLY
*     14-NOV-1985 (BDK):
*        change routine name from REQUEST
*     09-FEB-1988 (BDK):
*        trap overlength message strings
*     03-JUL-1990 (AJC):
*        add multi-line help facility
*        shorten names
*     25-NOV-1991 (BDK)
*        use ADAM_ACKNOW
*     14-JAN-1992 (AJC):
*        Report when setting STATUS
*      4-OCT-1992 (AJC):
*        Add missing space in message PROMPTCL5
*     22-FEB-1993 (AJC):
*        Use CHR_LEN not STR$TRIM
*      1-MAR-1993 (AJC):
*        Add INCLUDE DAT_PAR
*      8-MAR-1993 (AJC):
*        Replace include MESDEFNS and DDMSG with MESSYS_PAR,
*        INFINITE with MESSYS_INFINITE
*        and MSG_VAL_LEN with MESSYS__VAL_LEN
*      9-AUG-1993 (AJC):
*        INCLUDE SUBPAR_PARERR not PAR_ERR
*        Remove spurious statement STATUS = PAR__NULL
*      4-AUG-1994 (AJC)
*        Return PAR__NOUSR if no path
*     25-MAY-1995 (AJC)
*        Improve PAR__NOUSR message
*     21-NOV-1996 (AJC)
*        Define NULCHAR outside PARAMETER statement
*     13-JUN-2001 (AJC)
*        Call AMS (FAMS) directly, not via ADAM
*     11-JUL-2001 (AJC)
*        Make env var ADAM_NOPROMPT prevent prompting
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_PARERR'
      INCLUDE 'MESSYS_PAR'
      INCLUDE 'MESSYS_ERR'

*  Arguments Given:
      CHARACTER PARAM*(*)     ! name of parameter
      CHARACTER PROMPT*(*)    ! prompt string
      CHARACTER DFAULT*(*)    ! default value
      CHARACTER HLPTXT*(*)    ! one-line help text
      CHARACTER HLPKEY*(*)    ! full-help specifer
      CHARACTER ERRMES*(*)    ! error message

*  Arguments Returned:
      CHARACTER PARVAL*(*)    ! value obtained

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'

*  Local Constants:
      INTEGER TIMEOUT                    ! timeout on receive message
      PARAMETER ( TIMEOUT = MESSYS__INFINITE )
      INTEGER NOCONTEXT
      PARAMETER ( NOCONTEXT = 0 )
      CHARACTER NONAME*(*)
      PARAMETER ( NONAME = ' ' )
      CHARACTER NULCHAR*1                ! separator in messages -
                                         ! constant but set by assignment

*  Local Variables:
      INTEGER USRPATH     !  pointer to the path to the user interface
      INTEGER MESSID      !  message number of the RUN message
      INTEGER STRLEN      !  length given by STR$TRIM
      INTEGER VALPTR      !  pointer for assembly of value string
      INTEGER MSGSTATUS   !  message status returned
      INTEGER CONTEXT     !  context from GETREPLY (not used)
      CHARACTER*32 NAME   !  name from GETREPLY (not used)
      CHARACTER*80 TRERMS !  trimmed error message
      INTEGER LENERR      !  length of error message part

*    Local variables using parameter declared in 'DDMSG' :
      CHARACTER INVAL*(MESSYS__VAL_LEN)  !  used to assemble value string
      CHARACTER BUFF*(MESSYS__VAL_LEN)

*.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF

*   Define NULCHAR
      NULCHAR = CHAR ( 0 )
*
*   Obtain the path to the user interface which initiated this action,
*   along with the message identifier.
*
*   If ADAM_NOPROMPT is set, set USRPATH=0 to prevent prompt
      CALL PSX_GETENV( 'ADAM_NOPROMPT', BUFF, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         USRPATH = 0
      ELSE
         CALL EMS_ANNUL( STATUS )
         USRPATH = RUNPATH
         MESSID = RUNID
      ENDIF

*   If there is a path, construct and send the message
      IF ( USRPATH .GT. 0 ) THEN
*
*      Construct the value string ...
*
*      First the parameter keyword
         STRLEN = CHR_LEN( PARAM )
         IF ( STRLEN .EQ. 0 ) THEN
            BUFF(1:1) = ' '
            STRLEN = 1
         ELSE
            BUFF = PARAM(1:STRLEN)
         ENDIF

         INVAL = BUFF(1:STRLEN)//NULCHAR
         VALPTR = STRLEN + 2

*      Next the prompt string
         STRLEN = CHR_LEN( PROMPT )
         IF ( ( STRLEN .EQ. 0 ) .OR. ( BUFF(1:1) .EQ. NULCHAR ) ) THEN
            BUFF(1:1) = ' '
            STRLEN = 1
         ELSE
            BUFF = PROMPT(1:STRLEN)
         ENDIF
         INVAL(VALPTR:) = BUFF(1:STRLEN)//NULCHAR
         VALPTR = VALPTR + STRLEN + 1

*      Next the prompt value (default)
         STRLEN = CHR_LEN( DFAULT )
         IF ( ( STRLEN .EQ. 0 ) .OR. ( BUFF(1:1) .EQ. NULCHAR ) ) THEN
            BUFF(1:1) = ' '
            STRLEN = 1
         ELSE
            BUFF = DFAULT(1:STRLEN)
         ENDIF
         INVAL(VALPTR:) = BUFF(1:STRLEN)//NULCHAR
         VALPTR = VALPTR + STRLEN + 1

*      Calculate length of error message part
*      The error message must be sent - help is dispensible
         LENERR = CHR_LEN( ERRMES )
         IF ( ( LENERR .EQ. 0 ) .OR. ( TRERMS(1:1) .EQ. NULCHAR ) )
     :   THEN
            TRERMS(1:1) = ' '
            LENERR = 1
         ELSE
            TRERMS = ERRMES(1:LENERR)
         ENDIF

*      Next the one-line help
*      If there is insufficient room for the error message and null full
*      help, insert a null one-line help.
         STRLEN = CHR_LEN( HLPTXT )
         IF ( ( STRLEN .EQ. 0 ) .OR. ( BUFF(1:1) .EQ. NULCHAR ) .OR.
     :     ( VALPTR+STRLEN+1+LENERR+1+2 .GT. MESSYS__VAL_LEN ) ) THEN
            BUFF(1:1) = ' '
            STRLEN = 1
         ELSE
            BUFF = HLPTXT(1:STRLEN)
         ENDIF
         INVAL(VALPTR:) = BUFF(1:STRLEN)//NULCHAR
         VALPTR = VALPTR + STRLEN + 1

*      Next the full-help specifier.
*      If there is insufficient room for the error message, insert null
*      full help.
         STRLEN = CHR_LEN( HLPKEY )
         IF ( ( STRLEN .EQ. 0 ) .OR. ( BUFF(1:1) .EQ. NULCHAR ) .OR.
     :     ( VALPTR+STRLEN+1+LENERR+1 .GT. MESSYS__VAL_LEN ) ) THEN
            BUFF(1:1) = ' '
            STRLEN = 1
         ELSE
            BUFF = HLPKEY(1:STRLEN)
         ENDIF
         INVAL(VALPTR:) = BUFF(1:STRLEN)//NULCHAR
         VALPTR = VALPTR + STRLEN + 1

*      Insert the error message part. There will be room for it but, if not,
*      it will be silently truncated.
         IF ( VALPTR+LENERR+1 .GT. MESSYS__VAL_LEN ) THEN
            INVAL(VALPTR:) = TRERMS(1:MESSYS__VAL_LEN-VALPTR) // NULCHAR
            VALPTR = MESSYS__VAL_LEN
         ELSE
            INVAL(VALPTR:) = TRERMS(1:LENERR)//NULCHAR
            VALPTR = VALPTR + LENERR + 1
         ENDIF
*
*      construct the message and send it ...
*
          CALL FAMS_REPLY( USRPATH, MESSID, MESSYS__MESSAGE,
     :      MESSYS__PARAMREQ, NOCONTEXT, NONAME, MESSYS__VAL_LEN,
     :      INVAL, STATUS )
*
*      wait for reply to this specific message
*
          CALL FAMS_GETREPLY( TIMEOUT, USRPATH, MESSID, MSGSTATUS,
     :      CONTEXT, NAME, STRLEN, PARVAL, STATUS )
*
*      check the reply is of the right sort ...
*
         IF ( STATUS .EQ. SAI__OK ) THEN
            IF ( STRLEN .LT. LEN(PARVAL) )
     :        PARVAL(MAX(1,STRLEN+1):) = ' '

            IF ( MSGSTATUS .EQ. MESSYS__PARAMREP ) THEN
*
*         perform initial value testing ...
*
               IF ( PARVAL .EQ. '!' ) THEN
                  STATUS = PAR__NULL
                  CALL EMS_SETC( 'NAME', PARAM )
                  CALL EMS_REP( 'SUP_PROMPTCL2',
     :            'SUBPAR: Null (!) response to prompt for parameter'//
     :            ' ^NAME', STATUS)
               ELSE IF ( PARVAL .EQ. '!!' ) THEN
                  STATUS = PAR__ABORT
                  CALL EMS_SETC( 'NAME', PARAM )
                  CALL EMS_REP( 'SUP_PROMPTCL3',
     :            'SUBPAR: Abort (!!) response to prompt for ' //
     :            'parameter ^NAME', STATUS)
               ELSE
                  STATUS = SAI__OK
               ENDIF

            ELSE IF ( MSGSTATUS .EQ. MESSYS__TIMEOUT ) THEN

               STATUS = PAR__NOUSR
               CALL EMS_SETC( 'NAME', PARAM )
               CALL EMS_REP( 'SUP_PROMPTCL4',
     :         'SUBPAR: Timeout on reply to prompt for parameter ^NAME',
     :          STATUS )

            ELSE
*
*         wrong sort of reply; don't know what to do ...
*
               STATUS = PAR__NULL
               CALL EMS_SETC( 'NAME', PARAM )
               CALL EMS_REP( 'SUP_PROMPTCL5',
     :         'SUBPAR: Illegal message in reply to prompt '//
     :         'for parameter ^NAME', STATUS )

            ENDIF

         ENDIF
*   If there is no path for a prompt, return PAR__NOUSR
      ELSE

         STATUS = PAR__NOUSR
         CALL EMS_SETC( 'NAME', PARAM )
         CALL EMS_REP( 'SUP_PROMPTCL1',
     :   'SUBPAR: Parameter ^NAME has no value - prompting disallowed.',
     :    STATUS )

      ENDIF

      END
