      SUBROUTINE CAT5_RDTBL (FILE, SKIP, ROWS, NUMCOL, FDTYPA,
     :  FCSIZA, FSCLFA, FSCALA, FZEROA, FPOSNA, FANGLA, FNANGL,
     :  FPTRA, FPTRNA, STATUS)
*+
*  Name:
*     CAT5_RDTBL
*  Purpose:
*     Read in a free-format table of values.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT5_RDTBL (FILE, SKIP, ROWS, NUMCOL, FDTYPA, FCSIZA, FSCLFA,
*       FSCALA, FZEROA, FPOSNA, FANGLA, FNANGL, FPTRA, FPTRNA, STATUS)
*  Description:
*     Read in a free-format table of values.  The values are read from
*     the external file and held in local arrays.
*  Arguments:
*     FILE  =  CHARACTER (Given)
*        Full name of the file (including any directory specification).
*        If no directory specification is given the file is assumed to
*        be in the current directory.
*     SKIP  =  INTEGER (Given)
*        Number of records to skip before starting to read the table.
*     ROWS  =  INTEGER (Given)
*        Number of rows to be read from the table.
*     NUMCOL  =  INTEGER (Given)
*        Total number of columns in the table (treating vector
*        elements as separate columns).
*     FDTYPA(NUMCOL)  =  INTEGER (Given)
*        Data types of the columns.
*     FCSIZA(NUMCOL)  =  INTEGER (Given)
*        Size of character columns.
*     FSCLFA(NUMCOL)  =  LOGICAL (Given)
*        Flag indicating whether a scale factor and zero point are
*        to be applied to the column (.TRUE. if they are; othereise
*        .FALSE.).
*     FSCALA(NUMCOL)  =  DOUBLE PRECISION (Given)
*        Scale factor for the column.
*     FZEROA(NUMCOL)  =  DOUBLE PRECISION (Given)
*        Zero point for the column.
*     FPOSNA(NUMCOL)  =  INTEGER (Given)
*        Position of the column in the table.
*     FANGLA(NUMCOL)  =  INTEGER (Given)
*        Code indicating whether or not the column is an angle and
*        if so what its units are.
*     FNANGL(MAXCOL)  =  INTEGER (Given)
*        If the column is an angle then the corresponding array element
*        contains the sequence number of the column amongst the columns
*        of angles.  Otherwise zero.
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
*           Decompose the row into individual fields.
*           For every output identifier
*             If there is a corresponding field then
*               If the corresponding field is null then
*                 Set the null value flag for the field.
*               else
*                 Attempt to obtain the value for the field.
*               end if
*             else
*               Set the null value flag for the field.
*             end if
*           end for
*         end if
*       end for
*       Attempt to close the file.
*     else
*       Report an error opening the file.
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
*     12/7/96  (ACD): Original version.
*     28/8/96  (ACD): First stable version.
*     19/11/96 (ACD): Explicitly documented as reading free-format tables.
*     4/8/98   (ACD): Added options to allow for complex in addition
*        to simple angles.
*     18/11/98 (ACD): Changed text of error message.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'          ! External CAT constants.
      INCLUDE 'CAT1_PAR'         ! Internal CAT constants.
      INCLUDE 'CAT_ERR'          ! CAT error codes.
*  Global Variables:
      INCLUDE 'CAT5_STL_CMN'     ! Small text list common block.
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
*  Arguments Given:
      CHARACTER
     :  FILE*(*)
      INTEGER
     :  SKIP,
     :  ROWS,
     :  NUMCOL,
     :  FDTYPA(NUMCOL),
     :  FCSIZA(NUMCOL),
     :  FPOSNA(NUMCOL),
     :  FANGLA(NUMCOL),
     :  FNANGL(NUMCOL),
     :  FPTRA(NUMCOL),
     :  FPTRNA(NUMCOL)
      LOGICAL
     :  FSCLFA(NUMCOL)
      DOUBLE PRECISION
     :  FSCALA(NUMCOL),
     :  FZEROA(NUMCOL)
*  Status:
      INTEGER STATUS             ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      LOGICAL
     :  PRSOK       ! Flag; did record parse ok?
      CHARACTER
     :  BUFFER*(CAT1__SZDRC),    ! Input buffer for current line.
     :  ERRBUF*75,  ! Error message text.
     :  FLDLST(CAT5__MXCOL)*(CAT__SZVAL), ! list of fields in record.
     :  PRSMSG*75   ! Text of parser error message.
      INTEGER
     :  FUNIT,      ! Fortran unit number for the file.
     :  LSTAT,      ! Local I/O status.
     :  LFILE,      ! Length of FILE   (excl. trail. blanks).
     :  ERRLEN,     !   "    "  ERRBUF ( "  .   "  .   "   ).
     :  LOOP,       ! Loop index.
     :  ROW,        ! Current row.
     :  NFIELD,     ! Number of fields in the current record.
     :  CURCOL,     ! Current column.
     :  CURFLD      ! Current field.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Get a free Fortran unit number, attempt to open the file and
*       proceed if ok.

         CALL CAT1_GETLU (FUNIT, STATUS)
         OPEN(UNIT=FUNIT, STATUS='OLD', FILE=FILE, IOSTAT=LSTAT)
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
*          Read through the records constituting the table.

            DO ROW = 1, ROWS
               READ(FUNIT, 2000, IOSTAT=LSTAT) BUFFER
               CALL CAT1_IOERR (LSTAT, STATUS)

               IF (STATUS .EQ. CAT__OK) THEN

*
*                Decompose the row into individual fields.

                  CALL CAT1_DMRCD (BUFFER, CAT5__MXCOL, PRSOK, PRSMSG,
     :              NFIELD, FLDLST, STATUS)

*
*                Attempt to obtain a value for each column in the
*                catalogue.

                  DO CURCOL = 1, NUMCOL

*
*                   Check whether there is a field in the input record
*                   corresponding to this column.

                     CURFLD = FPOSNA(CURCOL)

C                    print5000, curcol, curfld
C5000                format(1x, 'CAT5_RDTBL - curcol, curfld: ', i5, i5)

                     IF (CURFLD .LE. NFIELD) THEN
C                       print5001, fldlst(curfld)
C5001                   format(1x, 'CAT5_RDTBL - fldlst(curfld): ', a )


*
*                      Check whether the corresponding field is null.

                        IF (FLDLST(CURFLD) .EQ. '<null>'  .OR.
     :                      FLDLST(CURFLD) .EQ. '<NULL>'  .OR.
     :                      FLDLST(CURFLD) .EQ. '?') THEN
                           CALL CAT5_STAEL (ROWS,
     :                       ROW, .TRUE.,
     :                       %VAL(CNF_PVAL(FPTRNA(CURCOL))), STATUS)
                        ELSE

*
*                         Attempt to obtain the value for the field.

                           CALL CAT5_GTVAL (FLDLST(CURFLD),
     :                       FDTYPA(CURCOL), FCSIZA(CURCOL),
     :                       FSCLFA(CURCOL),
     :                       FSCALA(CURCOL), FZEROA(CURCOL),
     :                       FANGLA(CURCOL), FNANGL(CURCOL),
     :                       FPTRA(CURCOL), FPTRNA(CURCOL),
     :                       ROWS, ROW, STATUS)
                        END IF

                     ELSE

*
*                      There is no field corresponding to the column
*                      in the field; set the null value flag.

                        CALL CAT5_STAEL (ROWS, ROW, .TRUE.,
     :                    %VAL(CNF_PVAL(FPTRNA(CURCOL))), STATUS)
                     END IF
                  END DO
               END IF
            END DO

*
*          Attempt to close the file.

            CLOSE(FUNIT, IOSTAT=LSTAT)
            IF (STATUS .EQ. CAT__OK) THEN
               CALL CAT1_IOERR (LSTAT, STATUS)
               IF (STATUS .NE. CAT__OK) THEN
                  CALL CAT1_ERREP ('CAT5_RDTBL_CLDF', 'Error: '/
     :              /'unable to close the file.', STATUS)
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

            CALL CHR_PUTC ('Failed to open catalogue data file ',
     :        ERRBUF, ERRLEN)

            IF (FILE .NE. ' ') THEN
               LFILE = CHR_LEN(FILE)
               CALL CHR_PUTC (FILE(1 : LFILE),  ERRBUF, ERRLEN)
            ELSE
               CALL CHR_PUTC ('<blank>', ERRBUF, ERRLEN)
            END IF

            CALL CHR_PUTC ('.', ERRBUF, ERRLEN)

            CALL CAT1_ERREP ('CAT5_RDTBL_OPDF', ERRBUF(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
