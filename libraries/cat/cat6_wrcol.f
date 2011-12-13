      SUBROUTINE CAT6_WRCOL (CI, TSUNIT, STATUS)
*+
*  Name:
*     CAT6_WRCOL
*  Purpose:
*     Write the column details, names and 'end-of-description' line for a
*     TST.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT6_WRCOL (CI, TSUNIT; STATUS)
*  Description:
*     Write the list of column names and the 'end-of-description' line for
*     a tab-separated table (TST).
*
*     The details written for each column are its units, data type and
*     external format.  These items are written as CURSA extensions to
*     the TST format; see SSN/75.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     TSUNIT  =  INTEGER (Given)
*        Fortran unit number for writing to the tab-separated table.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Set the separator character (a tab).
*     Initialise the column name, 'end of description', units, data
*     types and external formats buffers.
*     While there are more columns.
*       Increment the number of columns.
*       Attempt to obtain an identifier for the next column.
*       If ok and not the null identifier then
*         Get the column name.
*         Determine whether the column is a scalar or vector.
*         If the column is a scalar then
*           Append the column name to the name buffer.
*           Append dash characters to the 'end of description' buffer.
*           Append a tab-separator to the name buffer.
*           Append a tab-separator to the 'end of description' buffer.
*         else (the column is a vector)
*           Determine the number of elements.
*           For each element
*             Invent a name for the element.
*             Append the column name to the name buffer.
*             Append dash characters to the 'end of description' buffer.
*             Append a tab-separator to the name buffer.
*             Append a tab-separator to the 'end of description' buffer.
*           end for
*         end if
*         Do once for a scalar or for each vector element
*           Append the units to the units buffer.
*           Append a tab-separator to the units buffer.
*           Append the data type to the data types buffer.
*           Append a tab-separator to the data types buffer.
*           Append the external format to the external formats buffer.
*           Append a tab-separator to the external formats buffer.
*         end do
*       else
*         Set the termination flag.
*         If the status is not ok then
*           Report error.
*         end if
*       end if
*     end do
*     Remove any trailing tab-separators in the output buffers.
*     Write the output buffers.
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
*     11/7/00 (ACD): Added writing the following column details:
*        units, data types and external formats.
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
     :  TSUNIT
*  Status:
      INTEGER STATUS          ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Constants:
      CHARACTER DASH*1        ! Character for the 'end of description'
      PARAMETER (DASH = '-')  ! line.
*  Local Variables:
      CHARACTER
     :  TAB*1,                ! Tab-separator character.
     :  NAMBUF*(CAT6__SZDRC), ! Column names buffer.
     :  ENDBUF*(CAT6__SZDRC), ! 'End of deascription' buffer.
     :  UNTBUF*(CAT6__SZDRC), ! Column units buffer.
     :  TYPBUF*(CAT6__SZDRC), ! Column data types buffer.
     :  FMTBUF*(CAT6__SZDRC)  ! Column external formats buffer.
      CHARACTER
     :  FNAME*(CAT__SZCMP),   ! Name of the current column.
     :  ENAME*(CAT__SZCMP+4), ! Name of the current vector element.
     :  FUNITS*(CAT__SZUNI),  ! Units of the current column.
     :  FTYPE*10,             ! (Character) data type of the current column.
     :  FEXFMT*(CAT__SZEXF)   ! External format
      INTEGER
     :  NAMLEN,  ! Length of NAMBUF (excl. trail. blanks).
     :  ENDLEN,  !   "    "  ENDBUF ( "  .   "  .   "   ).
     :  UNTLEN,  !   "    "  UNTBUF ( "  .   "  .   "   ).
     :  TYPLEN,  !   "    "  TYPBUF ( "  .   "  .   "   ).
     :  FMTLEN,  !   "    "  FMTBUF ( "  .   "  .   "   ).
     :  LFNAME,  !   "    "  FNAME  ( "  .   "  .   "   ).
     :  LENAME,  !   "    "  ENAME  ( "  .   "  .   "   ).
     :  LFUNIT,  !   "    "  FUNITS ( "  .   "  .   "   ).
     :  LFTYPE,  !   "    "  FTYPE  ( "  .   "  .   "   ).
     :  LFEXFM   !   "    "  FEXFMT ( "  .   "  .   "   ).
      INTEGER
     :  FI,      ! Identifier for the current column.
     :  FDIM,    ! Column dimensionality attribute: scalar or vector.
     :  FSIZE,   ! Number of elements in a vector column.
     :  FCSIZE,  ! Size of character columns.
     :  FDTYPE,  ! (Integer code) data type of the current column.
     :  LSTAT,   ! Fortran I/O status.
     :  COLS,    ! Current column.
     :  LOOP,    ! Loop index.
     :  ELEM     ! Current vector element.
      LOGICAL
     :  MORE     ! Flag; more columsn to process?
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Set the separator character (a tab).

         TAB = CHAR(CAT6__TABI)

*
*       Initialise the column name, 'end of description', units, data
*       types and external formats buffers.

         NAMLEN = 0
         NAMBUF = ' '

         ENDLEN = 0
         ENDBUF = ' '

         UNTLEN = 15
         UNTBUF = '#column-units: '

         TYPLEN = 15
         TYPBUF = '#column-types: '

         FMTLEN = 17
         FMTBUF = '#column-formats: '

*
*       Process the columns.

         COLS = 0
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
*             Get the column name and determine whether it is a scalar or
*             vector.  Also get the units, data type and external format.
*             The data type is translated from an integer code to a
*             character string.

               CALL CAT_TIQAC (FI, 'NAME', FNAME, STATUS)
               CALL CAT_TIQAI (FI, 'DIMS', FDIM, STATUS)

               CALL CAT_TIQAC (FI, 'UNITS', FUNITS, STATUS)
               CALL CAT_TIQAC (FI, 'EXFMT', FEXFMT, STATUS)

               CALL CAT_TIQAI (FI, 'DTYPE', FDTYPE, STATUS)
               CALL CAT_TIQAI (FI, 'CSIZE', FCSIZE, STATUS)
               FTYPE = ' '
               LFTYPE = 0
               CALL CAT_TYFMT (FDTYPE, FCSIZE, FTYPE, LFTYPE, STATUS)

               IF (FTYPE(1 : 1) .EQ. '_') THEN
                  FTYPE(1 : 1) = ' '
               END IF

               CALL CHR_LDBLK (FTYPE)

               IF (FTYPE .NE. ' ') THEN
                  LFTYPE = CHR_LEN(FTYPE)
               END IF

*
*             Check whether the column is a scalar or vector.

               IF (FDIM .EQ. CAT__SCALR) THEN

*
*                Column is a scalar: append its name followed by a
*                tab-separator to the output buffer.  Append a
*                corresponding number of characters to the 'end of
*                description' buffer.

                  IF (FNAME .NE. ' ') THEN
                     LFNAME = CHR_LEN(FNAME)
                     CALL CHR_PUTC (FNAME(1 : LFNAME), NAMBUF, NAMLEN)

                     DO LOOP = 1, LFNAME
                        CALL CHR_PUTC (DASH, ENDBUF, ENDLEN)
                     END DO
                  END IF

                  CALL CHR_PUTC (TAB(1 : 1), NAMBUF, NAMLEN)
                  CALL CHR_PUTC (TAB(1 : 1), ENDBUF, ENDLEN)

                  FSIZE = 1

               ELSE

*
*                Column is a vector: Determine the number of elements
*                and then, for each element, form a name for it, add
*                this name to the output buffer and add a corresponding
*                number of characters to the 'end of description' buffer.

                  CALL CAT_TIQAI (FI, 'SIZE', FSIZE, STATUS)

                  IF (FNAME .NE. ' ') THEN
                     LFNAME = CHR_LEN(FNAME)
                  ELSE
                     LFNAME = 1
                  END IF

                  DO ELEM = 1, FSIZE
                     LENAME = 0
                     ENAME = ' '

                     IF (LFNAME .GT. 0) THEN
                        CALL CHR_PUTC (FNAME(1 : LFNAME), ENAME, LENAME)
                     END IF

                     CALL CHR_PUTC ('_', ENAME, LENAME)
                     CALL CHR_PUTI (ELEM, ENAME, LENAME)

                     CALL CHR_PUTC (ENAME(1 : LENAME), NAMBUF, NAMLEN)

                     DO LOOP = 1, LENAME
                        CALL CHR_PUTC (DASH, ENDBUF, ENDLEN)
                     END DO

                     CALL CHR_PUTC (TAB(1 : 1), NAMBUF, NAMLEN)
                     CALL CHR_PUTC (TAB(1 : 1), ENDBUF, ENDLEN)
                  END DO
               END IF

*
*             Assemble the units, data types and external formats buffers.
*             In the case of vector columns the items are replicated by
*             the number of elements in the vector.

               DO ELEM = 1, FSIZE
                  IF (FUNITS .NE. ' ') THEN
                     LFUNIT = CHR_LEN(FUNITS)
                     CALL CHR_PUTC (FUNITS(1 : LFUNIT), UNTBUF, UNTLEN)
                  END IF

                  CALL CHR_PUTC (TAB(1 : 1), UNTBUF, UNTLEN)

                  IF (FTYPE .NE. ' ') THEN
                     LFTYPE = CHR_LEN(FTYPE)
                     CALL CHR_PUTC (FTYPE(1 : LFTYPE), TYPBUF, TYPLEN)
                  END IF

                  CALL CHR_PUTC (TAB(1 : 1), TYPBUF, TYPLEN)

                  IF (FEXFMT .NE. ' ') THEN
                     LFEXFM = CHR_LEN(FEXFMT)
                     CALL CHR_PUTC (FEXFMT(1 : LFEXFM), FMTBUF, FMTLEN)
                  END IF

                  CALL CHR_PUTC (TAB(1 : 1), FMTBUF, FMTLEN)
               END DO

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
*       Remove any trailing tab-separators in the output buffers.

         IF (NAMLEN .GT. 0) THEN
            IF (NAMBUF(NAMLEN : NAMLEN) .EQ. TAB) THEN
               NAMBUF(NAMLEN : NAMLEN) = ':'
               NAMLEN = NAMLEN - 1
            END IF
         END IF

         IF (ENDLEN .GT. 0) THEN
            IF (ENDBUF(ENDLEN : ENDLEN) .EQ. TAB) THEN
               ENDBUF(ENDLEN : ENDLEN) = ':'
               ENDLEN = ENDLEN - 1
            END IF
         END IF

         IF (UNTLEN .GT. 0) THEN
            IF (UNTBUF(UNTLEN : UNTLEN) .EQ. TAB) THEN
               UNTBUF(UNTLEN : UNTLEN) = ':'
               UNTLEN = UNTLEN - 1
            END IF
         END IF

         IF (TYPLEN .GT. 0) THEN
            IF (TYPBUF(TYPLEN : TYPLEN) .EQ. TAB) THEN
               TYPBUF(TYPLEN : TYPLEN) = ':'
               TYPLEN = TYPLEN - 1
            END IF
         END IF

         IF (FMTLEN .GT. 0) THEN
            IF (FMTBUF(FMTLEN : FMTLEN) .EQ. TAB) THEN
               FMTBUF(FMTLEN : FMTLEN) = ':'
               FMTLEN = FMTLEN - 1
            END IF
         END IF

*
*       Write the output buffers.

         NAMLEN = MAX(NAMLEN, 1)
         ENDLEN = MAX(ENDLEN, 1)
         UNTLEN = MAX(UNTLEN, 1)
         TYPLEN = MAX(TYPLEN, 1)
         FMTLEN = MAX(FMTLEN, 1)

         WRITE(TSUNIT, 2000, IOSTAT=LSTAT)
     :     UNTBUF(1 : UNTLEN), TYPBUF(1 : TYPLEN), FMTBUF(1 : FMTLEN),
     :     NAMBUF(1 : NAMLEN), ENDBUF(1 : ENDLEN)
 2000    FORMAT(A / A / A // A / A)
         CALL CAT1_IOERR (LSTAT, STATUS)

      END IF

      END
