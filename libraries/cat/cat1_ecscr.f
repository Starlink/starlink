      SUBROUTINE CAT1_ECSCR (LINE, COMP, NAME, VALUE, MESSGE, FIRST,
     :  STATUS)
*+
*  Name:
*     CAT1_ECSCR
*  Purpose:
*     Report an error creating a component from items in a description file.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_ECSCR (LINE, COMP, NAME, VALUE, MESSGE; FIRST; STATUS)
*  Description:
*     Report an error creating a component from items in a description file.
*  Arguments:
*     LINE  =  INTEGER (Given)
*        Sequence number of the line in the description file.
*     COMP  =  CHARACTER*(*) (Given)
*        The type of component being created.  The value should be one
*        of: 'column', 'parameter' or 'directive'.
*     NAME  =  CHARACTER*(*) (Given)
*        The name of the item which generated the error.
*     VALUE  =  CHARACTER*(*) (Given)
*        The value of the item which generated the error.
*     MESSGE  =  CHARACTER*(*) (Given)
*        Brief message text summarising the error.
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
*     Assemble and output the the message.
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
*     8/7/96  (ACD): Original version.
*     9/7/96  (ACD): First stable version.
*     24/9/99 (ACD): Modified to report parser errors as errors rather
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
     :  COMP*(*),
     :  NAME*(*),
     :  VALUE*(*),
     :  MESSGE*(*)
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
     :  CMPLEN,     !   "    "  COMP   ( "  .   "  .   "   ).
     :  NAMLEN,     !   "    "  NAME   ( "  .   "  .   "   ).
     :  VALLEN,     !   "    "  VALUE  ( "  .   "  .   "   ).
     :  MSGLEN      !   "    "  MESSGE ( "  .   "  .   "   ).
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Set the status for reporting parser errors.

         PRSTAT = CAT__INVDS

*
*       If this is the first parser error in the description file then
*       output the 'invalid description file message'.

         IF (FIRST) THEN
            CALL CAT1_ERREP ('CAT1_ECSCR_E1',
     :        '*** Invalid description file:', PRSTAT)
            FIRST = .FALSE.
         END IF

*
*       Assemble and output the first line of the message.

         OUTBUF = ' '
         OUTLEN = 0

         CALL CHR_PUTC ('error near line ', OUTBUF, OUTLEN)
         CALL CHR_PUTI (LINE, OUTBUF, OUTLEN)
         CALL CHR_PUTC (' (', OUTBUF, OUTLEN)

         IF (COMP .NE. ' ') THEN
            CMPLEN = CHR_LEN(COMP)
         ELSE
            CMPLEN = 1
         END IF
         CALL CHR_PUTC (COMP(1 : CMPLEN), OUTBUF, OUTLEN)

         CALL CHR_PUTC (' defn): ', OUTBUF, OUTLEN)

         IF (NAME .NE. ' ') THEN
            NAMLEN = CHR_LEN(NAME)
         ELSE
            NAMLEN = 1
         END IF
         CALL CHR_PUTC (NAME(1 : NAMLEN), OUTBUF, OUTLEN)

         CALL CHR_PUTC ('=', OUTBUF, OUTLEN)

         IF (VALUE .NE. ' ') THEN
            VALLEN = CHR_LEN(VALUE)

            IF (VALLEN .LE. 10) THEN
               CALL CHR_PUTC (VALUE(1 : VALLEN), OUTBUF, OUTLEN)
            ELSE
               CALL CHR_PUTC (VALUE(1 : VALLEN), OUTBUF, OUTLEN)
               CALL CHR_PUTC ('...', OUTBUF, OUTLEN)
            END IF
         END IF

         CALL CHR_PUTC (' (', OUTBUF, OUTLEN)

         IF (MESSGE .NE. ' ') THEN
            MSGLEN = CHR_LEN(MESSGE)
            CALL CHR_PUTC (MESSGE(1 : MSGLEN), OUTBUF, OUTLEN)
         ELSE
            CALL CHR_PUTC ('<no message>', OUTBUF, OUTLEN)
         END IF

         CALL CHR_PUTC (').', OUTBUF, OUTLEN)

         CALL CAT1_ERREP ('CAT1_ECSCR_E2', OUTBUF(1 : OUTLEN), PRSTAT)

      END IF

      END
