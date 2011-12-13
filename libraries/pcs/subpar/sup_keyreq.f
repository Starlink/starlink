      SUBROUTINE SUBPAR_KEYREQ ( KEYWORD, STATUS )
*+
*  Name:
*     SUBPAR_KEYREQ

*  Purpose:
*     request confirmation of ambiguous keyword

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_KEYREQ ( KEYWORD, STATUS )

*  Description:
*     Prompts for a new keyword specification after an ambiguous one
*     has been given on the command line.

*  Arguments:
*     KEYWORD=CHARACTER*(*) (returned)
*        New keyword specification
*     STATUS=INTEGER
*        Global status

*  Algorithm:
*     Check whether the task is connected directly to the terminal or
*     not, and prompt in the appropriate way

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 1996, 2001 Central Laboratory of the Research Councils.
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
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-AUG-1994 (AJC):
*        Original
*     21-NOV-1996 (AJC):
*        Set NULLCHAR value by assignment
*     13-JUN-2001 (AJC)
*        Call AMS (FAMS) directly, not via ADAM
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

*  Arguments Returned:
      CHARACTER KEYWORD*(*)    ! keyword obtained

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'

*  Local Constants:
      INTEGER NOCONTEXT
      PARAMETER( NOCONTEXT = 0 )
      INTEGER PRLEN
      PARAMETER( PRLEN = 33 )
      CHARACTER*(*) PROMPT
      PARAMETER( PROMPT = 'Specify the keyword unambiguously' )
      CHARACTER*(*) NONAME
      PARAMETER( NONAME = ' ' )
      CHARACTER*1 NULLCHAR      ! message separator, constant but set
                                ! by assignment

*  Local Variables:
      INTEGER MSGSTATUS         ! Reply message status
      INTEGER KEYLEN            ! Reply message length
      INTEGER CONTEXT           ! Reply message context
      CHARACTER*32 NAME         ! Reply message name

*.

      IF ( STATUS .NE. SAI__OK ) RETURN
*
*   The common block logical variable RUNFACE will be set if the task
*   has been RUN by DCL rather than loaded as an ADAM task.
*
      IF ( RUNFACE .EQ. SUBPAR__TERM ) THEN
*
*      i/o goes directly to the terminal
*
         CALL ICL_READA( '  - '//PROMPT//' > ', PRLEN+7,
     :    PROMPT//' > ', PRLEN+7,
     :    KEYWORD, LEN(KEYWORD), ' ', 1 )

      ELSE IF ( RUNFACE .EQ. SUBPAR__TASK ) THEN
*
*      set message component separator
         NULLCHAR = CHAR( 0 )
*
*      send message to controlling task and get reply.
          CALL FAMS_REPLY( RUNPATH, RUNID, MESSYS__MESSAGE,
     :     MESSYS__PARAMREQ, NOCONTEXT, NONAME, MESSYS__VAL_LEN,
     :     ' '//NULLCHAR//
     :     PROMPT//NULLCHAR//
     :     NULLCHAR//' '//NULLCHAR//' '//NULLCHAR//' '//NULLCHAR,
     :     STATUS )

          CALL FAMS_GETREPLY( MESSYS__INFINITE, RUNPATH, RUNID,
     :      MSGSTATUS, CONTEXT, NAME, KEYLEN, KEYWORD, STATUS )

*      Check that we get the right type of reply
         IF ( STATUS .EQ. SAI__OK ) THEN
            IF ( KEYLEN .LT. LEN(KEYWORD) )
     :       KEYWORD(MAX(1,KEYLEN+1):) = ' '
            IF ( MSGSTATUS .NE. MESSYS__PARAMREP ) THEN
               STATUS = MSGSTATUS
            END IF
         END IF

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL EMS_REP( 'SUP_KEYREQ1',
     :      'SUBPAR: Error on prompt for un-ambiguous keyword',
     :      STATUS )
         END IF

      ELSE
*
*      A system error, task probably thinks it is a UTASK which should
*      never need to prompt.
*
         STATUS = PAR__NOUSR
      ENDIF

      END
