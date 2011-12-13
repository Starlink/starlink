      SUBROUTINE SUBPAR_SPLITVAL(VALUE,PARAM,PARLEN,PRSTR,PRLEN,
     :                           DFAULT,DEFLEN,HLPTXT,HLPLEN,
     :                           HLPKEY,HKYLEN,ERRMES,ERRLEN,
     :                           STATUS)
*+
*  Name:
*     SUBPAR_SPLITVAL

*  Purpose:
*     split value string for "askparam" components.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_SPLITVAL ( VALUE, PARAM, PARLEN,

*  Description:
*     Given the value string consisting of the parameter request components
*     separated by NUL characters, split it up.

*  Arguments:
*     VALUE=CHARACTER*(*)
*        message value string
*     PARAM=CHARACTER*(*)
*        parameter name
*     PARLEN=INTEGER
*        string length
*     PRSTR=CHARACTER*(*)
*        prompt string
*     PRLEN=INTEGER
*        string length
*     DFAULT=CHARACTER*(*)
*        default value
*     DEFLEN=INTEGER
*        string length
*     HLPTXT=CHARACTER*(*)
*        one-line help information on the parameter
*     HLPLEN=INTEGER
*        string length
*     HLPKEY=CHARACTER*(*)
*        fullhelp information on the parameter
*     HKYLEN=INTEGER
*        string length
*     ERRMES=CHARACTER*(*)
*        error message if any associated with this request
*     ERRLEN=INTEGER
*        string length
*     STATUS=INTEGER

*  Algorithm:
*     The value string contains substrings separated by nulls (as set up
*     by SUBPAR_PROMPTCL. The substrings are the parameter name, its prompt
*     string, its default value, its one-line help information, and an
*     error message if an earlier attempt to get the parameter has failed.
*     This information is put into variables which can be used by ASKPARAM.
*     The routine is expected to be called by the user interface ASKPARAM
*     routine.

*  Copyright:
*     Copyright (C) 1990, 1991 Science & Engineering Research Council.
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
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
*     08-JUN-1990 (AJC):
*        Original
*     17-JUL-1991 (AJC):
*        Use SAI__OK not ADAM__OK
*     21-NOV-1996 (AJC):
*        Set NULCHAR by assignment
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      CHARACTER VALUE*(*)           ! MESSAGE VALUE STRING

*  Arguments Returned:
      CHARACTER*(*) PARAM           ! parameter name
      INTEGER PARLEN                ! length of parameter name
      CHARACTER*(*) PRSTR           ! prompt string
      INTEGER PRLEN                 ! length of prompt string
      CHARACTER*(*) DFAULT          ! default value string
      INTEGER DEFLEN                ! length of default value string
      CHARACTER*(*) HLPTXT          ! help string
      INTEGER HLPLEN                ! length of help string
      CHARACTER*(*) HLPKEY          ! fullhelp string
      INTEGER HKYLEN                ! length of help string
      CHARACTER*(*) ERRMES          ! error message
      INTEGER ERRLEN                ! length of error message

*  Status:
      INTEGER STATUS

*  Local Constants:
      CHARACTER*1 NULCHAR           ! value component separator -
                                    ! constant but set by assignment

*  Local Variables:
      INTEGER STRPTR                ! pointer into the VALUE string
      INTEGER POSN                  ! pointer into the VALUE string

*.

      IF (STATUS .NE. SAI__OK) RETURN

*   Set NULCHAR value
      NULCHAR = CHAR(0)

*   initialise value string pointer ...
      STRPTR = 1

*   get parameter name ...
      POSN = INDEX(VALUE(STRPTR:),NULCHAR)
      PARAM = VALUE(STRPTR:(STRPTR+POSN-2))
      PARLEN = POSN - 1
      STRPTR = STRPTR + POSN

*   get parameter prompt string ...
      POSN = INDEX(VALUE(STRPTR:),NULCHAR)
      PRSTR = VALUE(STRPTR:(STRPTR+POSN-2))
      PRLEN = POSN - 1
      STRPTR = STRPTR + POSN
      IF (PRLEN.GT.0) THEN
         IF (PRSTR(1:PRLEN).EQ.' ') PRLEN = 0
      END IF

*   get default value string ...
      POSN = INDEX( VALUE(STRPTR:),NULCHAR)
      DFAULT = VALUE(STRPTR:(STRPTR+POSN-2))
      DEFLEN = POSN - 1
      STRPTR = STRPTR + POSN
      IF (DEFLEN.GT.0) THEN
         IF (DFAULT(1:DEFLEN).EQ.' ') DEFLEN = 0
      END IF

*   get one-line help string ...
      POSN = INDEX( VALUE(STRPTR:),NULCHAR)
      HLPTXT = VALUE(STRPTR:(STRPTR+POSN-2))
      HLPLEN = POSN - 1
      STRPTR = STRPTR + POSN
      IF (HLPLEN.GT.0) THEN
         IF (HLPTXT(1:HLPLEN).EQ.' ') HLPLEN = 0
      END IF

*   get fullhelp string ...
      POSN = INDEX( VALUE(STRPTR:),NULCHAR)
      HLPKEY = VALUE(STRPTR:(STRPTR+POSN-2))
      HKYLEN = POSN - 1
      STRPTR = STRPTR + POSN
      IF (HKYLEN.GT.0) THEN
         IF (HLPKEY(1:HKYLEN).EQ.' ') HKYLEN = 0
      END IF

*   get associated error message ...
      POSN = INDEX( VALUE(STRPTR:),NULCHAR)
      ERRMES = VALUE(STRPTR:(STRPTR+POSN-2))
      ERRLEN = POSN - 1
      IF (ERRLEN.LE.0) THEN
         ERRLEN = 0
         ERRMES = ' '
      ELSE IF (VALUE(STRPTR:(STRPTR+POSN-2)).EQ.' ') THEN
         ERRLEN = 0
         ERRMES = ' '
      ELSE
         ERRMES = VALUE(STRPTR:(STRPTR+POSN-2))
         ERRLEN = POSN - 1
      END IF

      END
