      SUBROUTINE CAT6_DDSCR (CDNAME, CI, SKIP, ROWS, PARS, RACOL,
     :   DECCOL, RAUNIT, STATUS)
*+
*  Name:
*     CAT6_DDSCR
*  Purpose:
*     Read and tab-separated table.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_DDSCR (CDNAME, CI; SKIP, ROWS, PARS, RACOL, DECCOL,
*       RAUNIT; STATUS)
*  Description:
*     Read and decode a tab-separated table to get the definitions of the
*     columns and parameters etc. that it contains.
*  Arguments:
*     CDNAME  =  CHARACTER (Given)
*        Full name of the tab-separated table file (including any directory
*        specification).  If no directory specification is given
*        the file is assumed to be in the current directory.
*     CI  =  INTEGER (Given)
*        Identifier to the catalogue which the description file
*        describes.
*     SKIP  =  INTEGER (Returned)
*        Number of records (of description) to skip at the start of the
*        table.
*     ROWS  =  INTEGER (Returned)
*        Number of rows in the table.
*     PARS  =  INTEGER (Returned)
*        Number of parameters in the table.
*     RACOL  =  INTEGER (Returned)
*        Sequence number of column of Right Ascension (name 'RA').
*        If the column is absent a value of zero is returned.
*     DECCOL  =  INTEGER (Returned)
*        Sequence number of column of Declination (name 'DEC').
*        If the column is absent a value of zero is returned.
*     RAUNIT  =  INTEGER (Returned)
*        Code for the units of column RA, coded as follows:
*          hours:   CAT1__HOUR,
*          degrees: CAT1__DEG.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Get a free Fortran unit number for accessing the tab-separated table.
*     Attempt to open the tab-separated table file.
*     If ok then
*       Read and decode the tab-separated table.
*       Attempt to close the tab-separated table file.
*     else
*       Report an error opening the tab-separated table file.
*     end if
*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils
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
*     19/5/99 (ACD): Original version (from  CAT1_DDSCR).
*     18/6/99 (ACD): First stable version.
*     12/7/00 (ACD): Removed the 'Interpretation mode'.
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
      INTEGER
     :  CI
*  Arguments Returned:
      INTEGER
     :  SKIP,
     :  ROWS,
     :  PARS,
     :  RACOL,
     :  DECCOL,
     :  RAUNIT
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  TSUNIT,   ! Fortran unit number for the tab-separated table file.
     :  LSTAT,    ! Local I/O status.
     :  ERRLEN,   ! Length of ERRTXT (excl. trail. blanks).
     :  LCDNAM    !   "    "  CDNAME ( "  .   "  .   "   ).
      CHARACTER
     :  ERRTXT*75 ! Text of error message.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Get a free Fortran unit number, attempt to open the tab-separated
*       table file and proceed if ok.

         CALL CAT1_GETLU (TSUNIT, STATUS)
         OPEN(UNIT=TSUNIT, STATUS='OLD', FILE=CDNAME, IOSTAT=LSTAT)
         CALL CAT1_IOERR (LSTAT, STATUS)

         IF (STATUS .EQ. CAT__OK) THEN

*
*          Read and decode the tab-separated table.

            CALL CAT6_RDSCR (TSUNIT, CI, SKIP, ROWS, PARS, RACOL,
     :        DECCOL, RAUNIT, STATUS)

*
*          Attempt to close the tab-separated table file.

            CLOSE(TSUNIT, IOSTAT=LSTAT)
            CALL CAT1_IOERR (LSTAT, STATUS)
            IF (STATUS .NE. CAT__OK) THEN
               CALL CAT1_ERREP ('CAT1_DDSCR_CLDF', 'Error: '/
     :           /'unable to close the tab-separated table.', STATUS)
            END IF

         ELSE

*
*          Report an error opening the description file.

            ERRTXT = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('Failed to open tab-separated table ',
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
