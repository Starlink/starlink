      SUBROUTINE CAT1_EDSCR (LINE, BUFFER, PRSMSG, FIRST, STATUS)
*+
*  Name:
*     CAT1_EDSCR
*  Purpose:
*     Report an error parsing the description file.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_EDSCR (LINE, BUFFER, PRSMSG; FIRST; STATUS)
*  Description:
*     Report an error parsing the description file.
*  Arguments:
*     LINE  =  INTEGER (Given)
*        Sequence number of the line in the description file.
*     BUFFER  =  CHARACTER*(*) (Given)
*        Buffer containing the line which failed to parse.
*     PRSMSG  =  CHARACTER*(*) (Given)
*        Status text associated with the failure of the line to parse.
*     FIRST  =  LOGICAL (Given and Returned)
*        Flag indicating whether it is the first parser error which
*        is being reported.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Set the status for reporting parser errors.
*     If this is the first message then
*       Output the 'invalid description file message'.
*     end if
*     Assemble and output the first line of the message.
*     Assemble and output the second line of the message.
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
*     4/7/96  (ACD): Original version.
*     19/5/99 (ACD): Fixed spelling mistake in comments.
*     23/9/99 (ACD): Modified to report parser errors as errors rather
*        than messages.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  LINE
      CHARACTER
     :  BUFFER*(*),
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
     :  PRSTAT,     ! Status for reporting parser errors.
     :  OUTLEN,     ! Length of OUTBUF (excl. trail. blanks).
     :  BUFLEN,     !   "    "  BUFFER ( "  .   "  .   "   ).
     :  PRSLEN,     !   "    "  PRSMSG ( "  .   "  .   "   ).
     :  BUFSTP      ! Last element of BUFFER to be output.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Set the status for reporting parser errors.

         PRSTAT = CAT__INVDS

*
*       If this is the first parser error in the description file then
*       output the 'invalid description file message'.

         IF (FIRST) THEN
            CALL CAT1_ERREP ('CAT1_EDSCR_E1',
     :        '*** Invalid description file:', PRSTAT)
            FIRST = .FALSE.
         END IF

*
*       Assemble and output the first line of the message.

         OUTBUF = ' '
         OUTLEN = 0

         CALL CHR_PUTC ('error in line ', OUTBUF, OUTLEN)
         CALL CHR_PUTI (LINE, OUTBUF, OUTLEN)
         CALL CHR_PUTC (': ', OUTBUF, OUTLEN)

         IF (BUFFER .NE. ' ') THEN
            BUFLEN = CHR_LEN(BUFFER)
            BUFSTP = MIN(BUFLEN, 45)
            CALL CHR_PUTC (BUFFER(1 : BUFSTP), OUTBUF, OUTLEN)
         END IF

         CALL CHR_PUTC ('...', OUTBUF, OUTLEN)

         CALL CAT1_ERREP ('CAT1_EDSCR_E2', OUTBUF(1 : OUTLEN), PRSTAT)

*
*       Assemble and output the second line of the message.

         OUTBUF = ' '
         OUTLEN = 0

         CALL CHR_PUTC ('  (', OUTBUF, OUTLEN)

         IF (PRSMSG .NE. ' ') THEN
            PRSLEN = CHR_LEN(PRSMSG)
            CALL CHR_PUTC (PRSMSG(1 : PRSLEN), OUTBUF, OUTLEN)
         ELSE
            CALL CHR_PUTC ('<no message>', OUTBUF, OUTLEN)
         END IF

         CALL CHR_PUTC (').', OUTBUF, OUTLEN)

         CALL CAT1_ERREP ('CAT1_EDSCR_E3', OUTBUF(1 : OUTLEN), PRSTAT)

      END IF

      END
