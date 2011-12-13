      SUBROUTINE CAT1_DDSCR (CDNAME, CI, STATUS)
*+
*  Name:
*     CAT1_DDSCR
*  Purpose:
*     Read and decode a description file.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_DDSCR (CDNAME, CI; STATUS)
*  Description:
*     Read and decode a small text list or direct access binary catalogue
*     description file to get the definitions of the columns and
*     parameters etc. that it contains.
*  Arguments:
*     CDNAME  =  CHARACTER (Given)
*        Full name of the description file (including any directory
*        specification).  If no directory specification is given
*        the file is assumed to be in the current directory.
*     CI  =  INTEGER (Given)
*        Identifier to the catalogue which the description file
*        describes.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Get a free Fortran unit number for accessing the description file.
*     Attempt to open the description file.
*     If ok then
*       Read and decode the description file.
*       Attempt to close the description file.
*     else
*       Report an error opening the description file.
*     end if
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
*     3/7/96   (ACD): Original version.
*     4/7/96   (ACD): First stable version.
*     17/11/98 (ACD): Improved error reporting.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
*  Arguments Given:
      CHARACTER
     :  CDNAME*(*)
*  Arguments Returned:
      INTEGER
     :  CI
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  DFUNIT,   ! Fortran unit number for the description file.
     :  LSTAT,    ! Local I/O status.
     :  ERRLEN,   ! Length of ERRTXT (excl. trail. blanks).
     :  LCDNAM    !   "    "  CDNAME ( "  .   "  .   "   ).
      CHARACTER
     :  ERRTXT*75 ! Text of error message.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Get a free Fortran unit number, attempt to open the description
*       file and proceed if ok.

         CALL CAT1_GETLU (DFUNIT, STATUS)
         OPEN(UNIT=DFUNIT, STATUS='OLD', FILE=CDNAME, IOSTAT=LSTAT)
         CALL CAT1_IOERR (LSTAT, STATUS)

         IF (STATUS .EQ. CAT__OK) THEN

*
*          Read and decode the description file.

            CALL CAT1_RDSCR (DFUNIT, CI, STATUS)

*
*          Attempt to close the description file.

            CLOSE(DFUNIT, IOSTAT=LSTAT)
            CALL CAT1_IOERR (LSTAT, STATUS)
            IF (STATUS .NE. CAT__OK) THEN
               CALL CAT1_ERREP ('CAT1_DDSCR_CLDF', 'Error: '/
     :           /'unable to close the description file.', STATUS)
            END IF

         ELSE

*
*          Report an error opening the description file.

            ERRTXT = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('Failed to open description file ',
     :        ERRTXT, ERRLEN)

            IF (CDNAME .NE. ' ') THEN
               LCDNAM = CHR_LEN(CDNAME)
               CALL CHR_PUTC (CDNAME(1 : LCDNAM), ERRTXT, ERRLEN)
            ELSE
               CALL CHR_PUTC ('<blank>', ERRTXT, ERRLEN)
            END IF

            CALL CHR_PUTC ('.', ERRTXT, ERRLEN)

            CALL CAT1_ERREP ('CAT1_DDSCR_OPDF', ERRTXT(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
