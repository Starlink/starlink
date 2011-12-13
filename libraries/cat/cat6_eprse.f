      SUBROUTINE CAT6_EPRSE (LINE, PRSMSG, FIRST, STATUS)
*+
*  Name:
*     CAT6_EPRSE
*  Purpose:
*     Report an error parsing the tab-separated table.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_EDSCR (LINE, PRSMSG; FIRST; STATUS)
*  Description:
*     Report an error parsing the tab-separated table.
*
*     Note that parser errors are reported as messages rather than
*     errors because no bad running status has been set.  The displayed
*     message starts with an exclamation mark for consistency with
*     other errors (and because the GUI expects one).
*  Arguments:
*     LINE  =  INTEGER (Given)
*        Sequence number of the line in the description file.  If a
*        negative or zero li9ne number is given it is not incorporated
*        into the message.
*     PRSMSG  =  CHARACTER*(*) (Given)
*        Status text associated with the failure of the line to parse.
*     FIRST  =  LOGICAL (Given and Returned)
*        Flag indicating whether it is the first parser error which
*        is being reported.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If this is the first message then
*       Output the 'invalid description file message'.
*     end if
*     Assemble and output the message.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
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
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     19/5/99 (ACD): Original version (from CAT1_EDSCR).
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
*  Arguments Given:
      INTEGER
     :  LINE
      CHARACTER
     :  PRSMSG*(*)
*  Arguments Given and Returned:
      LOGICAL
     :  FIRST
*  Status:
      INTEGER STATUS              ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      CHARACTER
     :  OUTBUF*80   ! Output buffer.
      INTEGER
     :  OUTLEN,     ! Length of OUTBUF (excl. trail. blanks).
     :  PRSLEN      !   "    "  PRSMSG ( "  .   "  .   "   ).
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       If this is the first parser error in the description file then
*       output the 'invalid description file message'.

         IF (FIRST) THEN
            CALL MSG_OUT (' ', '!*** Invalid tab-separated table:',
     :        STATUS)
            FIRST = .FALSE.
         END IF

*
*       Assemble and output the message.

         OUTBUF = ' '
         OUTLEN = 0

         CALL CHR_PUTC ('! ', OUTBUF, OUTLEN)

         IF (LINE .GT. 0) THEN
            CALL CHR_PUTC ('error in line ', OUTBUF, OUTLEN)
            CALL CHR_PUTI (LINE, OUTBUF, OUTLEN)
            CALL CHR_PUTC (': ', OUTBUF, OUTLEN)
         END IF

         IF (PRSMSG .NE. ' ') THEN
            PRSLEN = CHR_LEN(PRSMSG)
            CALL CHR_PUTC (PRSMSG(1 : PRSLEN), OUTBUF, OUTLEN)
         ELSE
            CALL CHR_PUTC ('<no message>', OUTBUF, OUTLEN)
         END IF

         CALL MSG_OUT (' ', OUTBUF(1 : OUTLEN), STATUS)

      END IF

      END
