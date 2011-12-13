      SUBROUTINE SUBPAR_SELHELP ( PARAM, INVAL, SHELP, HLPKEY,
     :                         HELPLB, LIBLEN, STRING, STRLEN,
     :                         LBSRCH, STATUS)
*+
*  Name:
*     SUBPAR_SELHELP

*  Purpose:
*     Select the required parameter help.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_SELHELP( PARAM, INVAL, SHELP, HLPKEY,
*                       HELPLB, LIBLEN, STRING, STRLEN,
*                       LBSRCH, STATUS)

*  Description:
*     Depending on the help specifiers given and the response to the
*     prompt, the appropriate returns are made in HELPLB, STRING and
*     LBSRCH.
*     If one-line help is to be output, return
*        HELPLB=' ', STRING=' '.
*     If help library module to be output, return
*        HELPLB=library name, STRING=key string.
*     If an error message is to be output, return
*        HELPLB=' ', STRING=error message.
*     LBSRCH is intended to indicate whether or not the help system
*     should prompt for more keys before returning to the parameter
*     prompt. It is set TRUE if a help library module is to be output
*     in response to '??'.

*  Arguments:
*     PARAM=CHARACTER*(*) (Given)
*        the name of the parameter involved (exact length)
*     INVAL=CHARACTER*(*) (Given)
*        the reply to the prompt (begins ?) (exact length)
*     SHELP=CHARACTER*(*) (Given)
*        the single-line help specifier (exact length)
*     HLPKEY=CHARACTER*(*) (Given)
*        the full help specifier (exact length)
*     HELPLB=CHARACTER*(*) (Returned)
*        The name of the library if a library module is required.
*     LIBLEN=INTEGER (Returned)
*        The used length of HELPLIB.
*     STRING=CHARACTER*(*) (Returned)
*        Either the string of keys within the library
*        or an error message.
*     STRLEN=CHARACTER*(*) (Returned)
*        The used length of STRING
*     LBSRCH=LOGICAL (Returned)
*        True if a library search is required.
*     STATUS=INTEGER

*  Algorithm:
*     The help specifiers will have been extracted from the task's
*     interface file 'help' and 'helplib'/'helpkey' fields for the
*     parameter.
*          '?'  => if 'help' is specified, return blank HELPLB
*                    and STRING unless it begins with % in which case
*                    interpret the remainder as for 'helpkey'
*                   if there is no 'help' specifier but there is a
*                   'helpkey' specifer, use that.
*          '??' => if 'helpkey' is specified, use it.
*                  if there is no 'helpkey' specifier, use 'help'
*                  specifier, if any.
*     If multi-line help is indicated in response to a single '?', set
*     LBSRCH false. If it is indicated in response to '??', set LBSRCH
*     true to indicate that a search of the library is required.

*  Copyright:
*     Copyright (C) 1990, 1992 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     04-JUN-1990 (AJC):
*        Original version
*     29-JAN-1992 (AJC):
*        Remove DEFAULTFILE from check for existence
*      7-MAY-1992 (AJC):
*        Leave file existence check to SUBPAR_WRHELP
*     28-JUN-1995 (AJC):
*        Declare LBSRCH LOGICAL

*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      CHARACTER PARAM*(*)           ! name of parameter
      CHARACTER INVAL*(*)           ! response to prompt
      CHARACTER SHELP*(*)           ! the 'help' specifier
      CHARACTER HLPKEY*(*)          ! the 'fullhelp' specifier

*  Arguments Returned:
      CHARACTER*(132) HELPLB        ! Help library
      INTEGER LIBLEN                ! Used length of HELPLB
      CHARACTER*(132) STRING        ! Help library keys or error msg
      INTEGER STRLEN                ! Used length of STRING
      LOGICAL LBSRCH                ! Whether to remain in help


*  Status:
      INTEGER STATUS

*  Global Variables:
*     <none>

*  Local Constants:
*     <none>
*    External routines :
      INTEGER CHR_LEN               ! Used length of string
      EXTERNAL CHR_LEN

*  Local Variables:
      INTEGER PARLEN                ! length of parameter name
      INTEGER HLPLEN                ! length of 'help' specifier
      INTEGER HKYLEN                ! length of 'helpkey' specifier
      INTEGER ENDLN                 ! length of inval
      INTEGER IP                    ! Start of help string info
      INTEGER SPACE                 ! Index lib/key separator in help string
      CHARACTER*(132) THELP         ! Local container for help library spec
      INTEGER TLEN                  ! Used length of THELP
      LOGICAL HELP                  ! Whether 'help' is specified
      LOGICAL FLHELP                ! Whether 'helpkey' is specified
      LOGICAL PCENT                 ! Whether 'help' specifier begins with %
      LOGICAL SINGLE                ! Whether to output single-line help
      LOGICAL FULL                  ! Whether to output multi-line help

*.


      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF

*   Get the lengths of the CHARACTER parameters
      ENDLN = LEN( INVAL )
      PARLEN = LEN( PARAM )
      HLPLEN = LEN( SHELP )
      HKYLEN = LEN( HLPKEY )

*   Now set flags to indicate presence or absence of specifiers
      IF ( SHELP(1:HLPLEN) .EQ. ' ' ) THEN
         PCENT = .FALSE.
         HELP = .FALSE.

      ELSE
         IP = 1
         CALL CHR_FIWS( SHELP(1:HLPLEN), IP, STATUS )
         IF( SHELP(IP:IP) .EQ. '%' ) THEN
            PCENT = .TRUE.
            HELP = .FALSE.
         ELSE
            PCENT = .FALSE.
            HELP = .TRUE.
         ENDIF

      ENDIF

      IF (HLPKEY(1:HKYLEN) .EQ. ' ' ) THEN
         FLHELP = .FALSE.
      ELSE
         FLHELP = .TRUE.
      ENDIF

*   Now decide what type of help to output.
*      Set SINGLE true if single-line help is required: otherwise false
*      Set FULL true if full help is required: otherwise false
*      SET LBSRCH = .TRUE.  if it is required to stay and search the library
*                 = .FALSE. if to return to the parameter prompt immediately
      IF ((.NOT.HELP) .AND.
     :    (.NOT.FLHELP) .AND.
     :    (.NOT.PCENT)) THEN
         HELPLB = ' '
         STRING = 'Sorry, help is not specified for parameter '//
     :                  PARAM(1:PARLEN)
         FULL = .FALSE.
         SINGLE = .FALSE.

      ELSEIF (INVAL(1:ENDLN) .EQ. '?' ) THEN
         IF ( HELP ) THEN
            SINGLE = .TRUE.
            FULL = .FALSE.

         ELSE
            SINGLE = .FALSE.
            FULL = .TRUE.
            LBSRCH = .FALSE.
         ENDIF

      ELSEIF ( INVAL(1:ENDLN) .EQ. '??' ) THEN
         IF ( FLHELP ) THEN
            SINGLE = .FALSE.
            PCENT = .FALSE.
            FULL = .TRUE.
            LBSRCH = .TRUE.

         ELSEIF ( PCENT ) THEN
            SINGLE = .FALSE.
            FULL = .TRUE.
            LBSRCH = .TRUE.

         ELSE
*         Full help is not available, use one-line
            SINGLE = .TRUE.
            FULL = .FALSE.
         ENDIF

      ENDIF

*   Now output the required type of help
      IF ( SINGLE ) THEN
         HELPLB = ' '
         STRING = ' '

      ELSEIF ( FULL ) THEN
*      Full help required.
*           PCENT is true if it is due to % form of single line help
*           LBSRCH is set if library searching is required.
         IF ( PCENT ) THEN
            IP = IP + 1
            THELP = SHELP(IP:HLPLEN)
            TLEN = HLPLEN - IP + 1

         ELSE
            IP = 1
            CALL CHR_FIWS( HLPKEY(1:HKYLEN), IP, STATUS )
            THELP = HLPKEY(IP:HKYLEN)
            TLEN = HKYLEN - IP + 1
         ENDIF

*      Set SPACE to lib/key separator
         SPACE = INDEX( THELP(1:TLEN), ' ' )
         IF (SPACE .GT. 0) THEN
*         keys present
            HELPLB = THELP(1:SPACE-1)
            LIBLEN = SPACE-1
            STRING = THELP(1+SPACE:TLEN)
            STRLEN = TLEN-1-LIBLEN
         ELSE
*         no keys
            HELPLB = THELP(1:TLEN)
            LIBLEN = TLEN
            STRING = ' '
            STRLEN = 1
         ENDIF

      ENDIF

      LIBLEN = CHR_LEN( HELPLB )
      STRLEN = CHR_LEN( STRING )

      END
