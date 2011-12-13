      SUBROUTINE CAT6_WRTBL (CI, ROWS, TSUNIT, STATUS)
*+
*  Name:
*     CAT6_WRTBL
*  Purpose:
*     Write a tab-separated table of values.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT6_WRTBL (CI, ROWS, TSUNIT; STATUS)
*  Description:
*     Write a tab-separated table of values.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     ROWS  =  INTEGER (Given)
*        Number of rows in the catalogue.
*     TSUNIT  =  INTEGER (Given)
*        Fortran unit number for writing to the tab-separated table.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Set the separator character (a tab).
*     Initialise the list of fields.
*     While there are more columns
*       Increment the number of columns.
*       Attempt to get an identifier for the columns.
*       If all ok and the identifier is not the null identifier then
*         Determine whether the column is a scalar or vector.
*         If the column is a scalar then
*           Add the identifier to the list.
*         else (the column is a vector)
*           Get the name of the column.
*           Get the number of elements.
*           For every element
*             Assemble the name of the element.
*             Get an identifier for the element.
*             If ok then
*               Add the identifier to the list.
*             end if
*           end for
*         end if
*       else
*         Set the termination flag.
*         If the status is not ok then
*           Report error.
*         end if
*       end if
*     end do
*     If there were too many fields then
*       Report message.
*     end if
*     If all is ok then
*       For every row
*         Read the current row.
*         Initialise the output buffer.
*         For every field
*           Get the value.
*           Append the value to the output buffer.
*           Append a tab-separator to the output buffer.
*         end for
*         Remove any trailing tab-separator.
*         Write the row to the output file.
*       end for
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
*     16/9/99 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
*  Global Variables:
      INCLUDE 'CAT6_TST_CMN'      ! TST back-end common block.
*  Arguments Given:
      INTEGER
     :  CI,
     :  ROWS,
     :  TSUNIT
*  Status:
      INTEGER STATUS          ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      CHARACTER
     :  TAB*1,                 ! Tab-separator character.
     :  FNAME*(CAT__SZCMP),    ! Name of the current column.
     :  ENAME*(CAT__SZCMP+5),  ! Name of the current vector element.
     :  BUFFER*(CAT6__SZDRC),  ! Output buffer.
     :  VALUE*(CAT__SZVAL)     ! Value for the current field.
      INTEGER
     :  NFLD,    ! Number of fields.
     :  FIELDS(CAT6__MXCOL),   ! List of field identifiers.
     :  FI,      ! Identifier for the current column.
     :  FIE,     ! Identifier for the current vector element.
     :  FDIM,    ! Column dimensionality attribute: scalar or vector.
     :  FSIZE,   ! Number of elements in a vector column.
     :  COLS,    ! Current column.
     :  FLD,     ! Current field.
     :  ELEM,    ! Current vector element.
     :  ROW      ! Current row.
      INTEGER
     :  BUFLEN,  ! Length of NAMBUF (excl. trail. blanks).
     :  LFNAME,  !   "    "  FNAME  ( "  .   "  .   "   ).
     :  LENAME,  !   "    "  ENAME  ( "  .   "  .   "   ).
     :  LVALUE,  !   "    "  VALUE  ( "  .   "  .   "   ).
     :  LSTAT    ! Fortran I/O status.
      LOGICAL
     :  MORE,    ! Flag; more columsn to process?
     :  TMFLDS,  ! Flag; too many fields?
     :  NULFLG   ! Flag; is the current field null?
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Set the separator character (a tab).

         TAB = CHAR(CAT6__TABI)

*
*       Initialise the list of fields.

         NFLD = 0

*
*       Assemble a list of identifiers for each field.  This list
*       comprises identifiers for scalar columns and vector column
*       elements.

         COLS = 0
         TMFLDS = .FALSE.
         MORE = .TRUE.

         DO WHILE (MORE)

*
*          Increment the number of columns and attempt to obtain an
*          identifier for the next column.  Then proceed if all is ok
*          and the identifier is not the null identifier.

            COLS = COLS + 1
            CALL CAT_TNDNT (CI, CAT__FITYP, COLS, FI, STATUS)

            IF (STATUS .EQ. CAT__OK  .AND.  FI .NE. CAT__NOID) THEN

*
*             Determine whether the column scalar or vector and proceed
*             accordingly.

               CALL CAT_TIQAI (FI, 'DIMS', FDIM, STATUS)

               IF (FDIM .EQ. CAT__SCALR) THEN

*
*                The column is a scalar: add the identifier to the list.

                  IF (NFLD .LT. CAT6__MXCOL) THEN
                     NFLD = NFLD + 1
                     FIELDS(NFLD) = FI
                  ELSE
                     TMFLDS = .TRUE.
                  END IF

               ELSE

*
*                The column is a vector: get the name of the column
*                and the number of elements.  For each element get
*                the identifier and add it to the list.

                  CALL CAT_TIQAC (FI, 'NAME', FNAME, STATUS)
                  CALL CAT_TIQAI (FI, 'SIZE', FSIZE, STATUS)

                  IF (FNAME .NE. ' ') THEN
                     LFNAME = CHR_LEN(FNAME)
                  ELSE
                     LFNAME = 1
                  END IF

                  DO ELEM = 1, FSIZE
                     LENAME = 0
                     ENAME = ' '

                     CALL CHR_PUTC (FNAME(1 : LFNAME), ENAME, LENAME)
                     CALL CHR_PUTC ('[', ENAME, LENAME)
                     CALL CHR_PUTI (ELEM, ENAME, LENAME)
                     CALL CHR_PUTC (']', ENAME, LENAME)

                     CALL CAT_TIDNT (CI, ENAME(1 : LENAME), FIE, STATUS)

                     IF (STATUS .EQ. CAT__OK) THEN
                        IF (NFLD .LT. CAT6__MXCOL) THEN
                           NFLD = NFLD + 1
                           FIELDS(NFLD) = FIE
                        ELSE
                          TMFLDS = .TRUE.
                        END IF
                     END IF
                  END DO
               END IF
            ELSE

*
*             Failed to get a column identifier.  Set the termination flag
*             and report any error.

               MORE = .FALSE.

               IF (STATUS .NE. CAT__OK) THEN
                  CALL CAT1_ERREP ('CAT6_WRCOL_ERR', 'Failed to '/
     :              /'get column identifier.', STATUS)
               END IF
            END IF
         END DO

*
*       If there were too many fields then report a message.

         IF (TMFLDS) THEN
            CALL CAT1_MSG (' ', 'Too many columns for a '/
     :        /'tab-separated table; some will be omitted.', STATUS)
         END IF

*
*       If all is ok then proceed to write the table.

         IF (STATUS .EQ. CAT__OK) THEN

            DO ROW = 1, ROWS

*
*             Read the current row.

               CALL CAT_RGET (CI, ROW, STATUS)

*
*             Initialise the output buffer.

               BUFLEN = 0
               BUFFER = ' '

*
*             For every field in the row, get the value and append it
*             to the buffer.

               DO FLD = 1, NFLD
                  CALL CAT_EGT0F (FIELDS(FLD), VALUE, NULFLG, STATUS)

                  IF (.NOT. NULFLG) THEN
                     IF (VALUE .NE. ' ') THEN
                        LVALUE = CHR_LEN(VALUE)
                     ELSE
                        LVALUE = 1
                     END IF

                     CALL CHR_PUTC (VALUE(1 : LVALUE), BUFFER, BUFLEN)
                  END IF

*
*                Append a tab-separator to the output buffer.

                  CALL CHR_PUTC (TAB, BUFFER, BUFLEN)
               END DO

*
*             Remove any trailing tab-separator.

               IF (BUFFER(BUFLEN : BUFLEN) .EQ. TAB) THEN
                  BUFFER(BUFLEN : BUFLEN) = ' '
                  BUFLEN = BUFLEN - 1
               END IF

*
*             Write the row to the output file.

               BUFLEN = MAX(BUFLEN, 1)

               WRITE(TSUNIT, 2000, IOSTAT=LSTAT) BUFFER(1 : BUFLEN)
 2000          FORMAT(A)
               CALL CAT1_IOERR (LSTAT, STATUS)

            END DO
         END IF

      END IF

      END
