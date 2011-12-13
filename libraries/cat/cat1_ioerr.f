      SUBROUTINE CAT1_IOERR (FSTAT, STATUS)
*+
*  Name:
*     CAT1_IOERR
*  Purpose:
*     Translate and report a Fortran I/O error.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_IOERR (FSTAT; STATUS)
*  Description:
*     Translate and report a Fortran I/O error.
*
*     Note that this routine attempts to execute, irrespective of the
*     status on input.
*  Arguments:
*     FSTAT  =  INTEGER (Given)
*        Fortran I/O error status.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     <...>
*  Implementation Deficiencies:
*     This version of the routine is something of a kludge.
*
*     Also, this routine has the Fortran I/O statuses (sic)
*     corresponding to success and end-of-file hardwired as a local
*     parameter.  I think that, technically, this is standard and
*     portable, but it is a potential problem area.
*
*     Note in passing (PWD 23/06/04) the Fortran standard defines EOF as
*     any return from IOSTAT less than zero, so this may not trap all
*     EOF cases.  However, most compilers return -1 for EOF and use
*     values less than this to indicate other returns, -2, end of record
*     (usually an internal read), -4 end of record (usually external
*     read). Anyway left this alone.
*
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
*     ACD: A C Davenhall (Leicester)
*  History:
*     28/6/93  (ACD): Original version.
*     4/7/93   (ACD): First working version.
*     23/1/94  (ACD): Modified error reporting.
*     5/6/98   (ACD): Changed that CAT status which is set from
*       CAT__ERROR to CAT__IOERR.
*     18/11/98 (ACD): Changed translation of status to use FIO_SERR.
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
     :  FSTAT
*  Status:
      INTEGER STATUS             ! Global status
*  Local Constants:
      INTEGER FIOOK      ! Success status for Fortran I/O.
      PARAMETER (FIOOK = 0)
*
      INTEGER EOF        ! Fortran I/O status for end-of-file.
      PARAMETER (EOF = -1)
*  Local Variables:
      CHARACTER
     :  BUFFER*80  ! Output buffer for message.
      INTEGER
     :  BUFLEN     ! Length of BUFFER (excl. trail. blanks).
*.

*
*    Note the this routine attempts to execute irrespective of the
*    running status on input,
*
*    Also note that though it will attempt to set the return status
*    if the Fortran I/O status is not ok, it will not over-write a
*    previously bad CAT status.

      IF (FSTAT .NE. FIOOK) THEN

*
*       Check for end-of-file and handle it as a special case.

         IF (FSTAT .EQ. EOF) THEN
            IF (STATUS .EQ. CAT__OK) THEN
               STATUS = CAT__EOF
            END IF

         ELSE
            IF (STATUS .EQ. CAT__OK) THEN
               CALL FIO_SERR (FSTAT, STATUS)

               BUFFER = ' '
               BUFLEN = 0

               CALL CHR_PUTC ('  (corresponding to Fortran I/O error '/
     :           /'code ', BUFFER, BUFLEN)
               CALL CHR_PUTI (FSTAT, BUFFER, BUFLEN)
               CALL CHR_PUTC (').', BUFFER, BUFLEN)

               CALL CAT1_ERREP ('CAT1_IOERR_IO', BUFFER(1 : BUFLEN),
     :           STATUS)
            END IF

         END IF
      END IF

      END
