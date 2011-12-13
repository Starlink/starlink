      SUBROUTINE CAT6_RDTBL (CATFIL, SKIP, ROWS, RACOL, DECCOL, RAUNIT,
     :  NUMCOL, FDTYPA, FCSIZA, FPTRA, FPTRNA, STATUS)
*+
*  Name:
*     CAT6_RDTBL
*  Purpose:
*     Read in a the table for a tab-separated table.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT6_RDTBL (CATFIL, SKIP, ROWS, RACOL, DECCOL, RAUNIT,
*       NUMCOL, FDTYPA, FCSIZA, FPTRA, FPTRNA; STATUS)
*  Description:
*     Read in a the table for a tab-separated table (TST).  The description
*     at the start of the TST is skipped (having been previously read and
*     parsed).
*  Arguments:
*     CATFIL  =  CHARACTER (Given)
*        Full name of the file (including any directory specification)
*        containing the TST.  If no directory specification is given the
*        file is assumed to be in the current directory.
*     SKIP  =  INTEGER (Given)
*        Number of records to skip before starting to read the table.
*     ROWS  =  INTEGER (Given)
*        Number of rows to be read from the table.
*     RACOL  =  INTEGER (Given)
*        Sequence number of column of Right Ascension (name 'RA').
*        If the column is absent a value of zero is given.
*     DECCOL  =  INTEGER (Given)
*        Sequence number of column of Declination (name 'DEC').
*        If the column is absent a value of zero is given.
*     RAUNIT  =  INTEGER (Given)
*        Code for the units of column RA, coded as follows:
*          hours:   CAT1__HOUR,
*          degrees: CAT1__DEG.
*     NUMCOL  =  INTEGER (Given)
*        Total number of columns in the table (treating vector
*        elements as separate columns).
*     FDTYPA(NUMCOL)  =  INTEGER (Given)
*        Data types of the columns.
*     FCSIZA(NUMCOL)  =  INTEGER (Given)
*        Size of character columns.
*     FPTRA(NUMCOL)  =  INTEGER (Given)
*        Pointer to array to hold the column.
*     FPTRNA(NUMCOL)  =  INTEGER (Given)
*        Pointer to array to hold the null value flags correspnding to
*        the column.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Get a free Fortran unit number for accessing the file.
*     Attempt to open the file.
*     If ok then
*       For the number of header records to be skipped
*         Attempt to read a record.
*       end for
*       For every row in the table
*         Attempt to read the row
*         If ok then
*           Read the fields for the row.
*         end if
*       end for
*       Attempt to close the file.
*     else
*       Report an error opening the file.
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
*     4/6/99  (ACD): Original version (based on CAT5_RDTBL).
*     18/6/99 (ACD): First stable version.
*     13/7/00 (ACD): Removed the 'interpretation mode.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'          ! External CAT constants.
      INCLUDE 'CAT1_PAR'         ! Internal CAT constants.
      INCLUDE 'CAT_ERR'          ! CAT error codes.
*  Arguments Given:
      CHARACTER
     :  CATFIL*(*)
      INTEGER
     :  SKIP,
     :  ROWS,
     :  RACOL,
     :  DECCOL,
     :  RAUNIT,
     :  NUMCOL
      INTEGER
     :  FDTYPA(NUMCOL),
     :  FCSIZA(NUMCOL),
     :  FPTRA(NUMCOL),
     :  FPTRNA(NUMCOL)
*  Status:
      INTEGER STATUS          ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  FUNIT,    ! Fortran unit number for the file.
     :  LSTAT,    ! Local I/O status.
     :  LOOP,     ! Loop index.
     :  ROW,      ! Current row.
     :  ERRLEN,   ! Length of ERRBUF (excl. trail. blanks).
     :  LFILE     !   "    "  FILE   ( "  .   "  .    "  ).
      CHARACTER
     :  BUFFER*(CAT1__SZDRC), ! Input buffer for current line.
     :  ERRBUF*75             ! Error message text.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Get a free Fortran unit number, attempt to open the file and
*       proceed if ok.

         CALL CAT1_GETLU (FUNIT, STATUS)
         OPEN(UNIT=FUNIT, STATUS='OLD', FILE=CATFIL, IOSTAT=LSTAT)
         CALL CAT1_IOERR (LSTAT, STATUS)

         IF (STATUS .EQ. CAT__OK) THEN

*
*          Read through any records to be skipped at the head of the
*          file.

            DO LOOP = 1, SKIP
               READ(FUNIT, 2000, IOSTAT=LSTAT) BUFFER
 2000          FORMAT(A)
            END DO

*
*          Read and process the rows in the table.

            DO ROW = 1, ROWS

*
*             Attempt to read the next row and proceed if ok.

               READ(FUNIT, 2000, IOSTAT=LSTAT) BUFFER
               CALL CAT1_IOERR (LSTAT, STATUS)

               IF (STATUS .EQ. CAT__OK) THEN

*
*                Read the fields from the current row.

                  CALL CAT6_RDROW (RACOL, DECCOL, RAUNIT, ROWS, ROW,
     :              BUFFER, NUMCOL, FDTYPA, FCSIZA, FPTRA, FPTRNA,
     :              STATUS)
               END IF
            END DO

*
*          Attempt to close the file.

            CLOSE(FUNIT, IOSTAT=LSTAT)
            IF (STATUS .EQ. CAT__OK) THEN
               CALL CAT1_IOERR (LSTAT, STATUS)
               IF (STATUS .NE. CAT__OK) THEN
                  CALL CAT1_ERREP ('CAT6_RDTBL_CLDF', 'Failed to '/
     :              /'close the tab-separated table file.', STATUS)
               END IF
            END IF

         ELSE

*
*          Report an error opening the file.  Note that the file name
*          is included in the error message here because having the
*          wrong file name in the description file seems a likely error,
*          and including this name is a diagnostic aid.

            ERRLEN = 0
            ERRBUF = ' '

            CALL CHR_PUTC ('Failed to open tab-separated table file ',
     :        ERRBUF, ERRLEN)

            IF (CATFIL .NE. ' ') THEN
               LFILE = CHR_LEN(CATFIL)
               CALL CHR_PUTC (CATFIL(1 : LFILE),  ERRBUF, ERRLEN)
            ELSE
               CALL CHR_PUTC ('<blank>', ERRBUF, ERRLEN)
            END IF

            CALL CHR_PUTC ('.', ERRBUF, ERRLEN)

            CALL CAT1_ERREP ('CAT6_RDTBL_OPDF', ERRBUF(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
