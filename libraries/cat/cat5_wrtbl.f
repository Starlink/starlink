      SUBROUTINE CAT5_WRTBL (CI, ROWS, DFUNIT, STATUS)
*+
*  Name:
*     CAT5_WRTBL
*  Purpose:
*     Write table of values to the catalogue file.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT5_WRTBL (CI, ROWS, DFUNIT; STATUS)
*  Description:
*     Write table of values to the catalogue file.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     ROWS  =  INTEGER (Given)
*        Number of rows in the catalogue.
*     DFUNIT  =  INTEGER (Given)
*        Fortran unit number for writing to the catalogue file.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Attempt to determine the range of elements corresponding to the
*     given catalogue in the STL common blocks.
*     If ok then
*       For every row
*         Initialise the output buffer.
*         For every field
*           If the value is not null then
*             For the appropriate data type
*               Get the value from the array.
*               Attempt to write the value to the field using the
*               external format.
*             end for
*             Append the value to the output buffer.
*           else (the value is null)
*             Append '<null>' to the output buffer.
*           end if
*         end for
*         Attempt to write the buffer to the file.
*       end for
*     else
*       Set the status.
*       Report an error.
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
*     19/7/96 (ACD): Original version.
*     30/7/96 (ACD): First stable version.
*     6/6/98  (ACD): Changed the way that columns are identified.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Global Variables:
      INCLUDE 'CAT1_IDS_CMN'      ! Identifiers common block.
      INCLUDE 'CAT5_STL_CMN'      ! STL back-end common block.
      INCLUDE 'CNF_PAR'           ! For CNF_PVAL function
*  Arguments Given:
      INTEGER
     :  CI,
     :  ROWS,
     :  DFUNIT
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Constants:
      INTEGER IOOK           ! Fortran I/O success status.
      PARAMETER (IOOK = 0)
*  Local Variables:
      INTEGER
     :  FIELD,   ! Element for the current field.
     :  ROW,     ! Current row.
     :  FDTYPE,  ! Data type of the current field.
     :  FCSIZE,  ! Character size of the current field.
     :  PTR,     ! Pointer to column for current field.
     :  PTRN     ! Pointer to null value flags for the current field.
      CHARACTER
     :  BUFFER*(CAT1__SZDRC),  ! Buffer for the current record.
     :  FFMT*(CAT__SZEXF),     ! Format of the current field.
     :  FLDBUF*(CAT__SZVAL),   ! Character value of the current field.
     :  ERRMSG*75              ! Text of error message.
      INTEGER
     :  BUFLEN,  ! Length of BUFFER (excl. trail. blanks).
     :  FLDPOS,  !   "    "  FLDBUF ( "  .   "  .   "   ).
     :  ERRLEN,  !   "    "  ERRMSG ( "  .   "  .   "   ).
     :  LCVAL,   !   "    "  CVAL   ( "  .   "  .   "   ).
     :  LSTAT    ! Fortran I/O status.
      LOGICAL
     :  NULFLG,  ! Null value flag.
     :  MORE     ! Flag; required column found?
      INTEGER
     :  FIA(CAT5__MXCOL),   ! Column identifiers.
     :  FIAT(CAT5__MXCOL),  ! Column identifers sorted on position.
     :  POSN(CAT5__MXCOL),  ! Column positions.
     :  LOOP,    ! Loop index.
     :  NFIELD,  ! Number of columns.
     :  CFIELD,  ! Current column.
     :  DIM,     ! Dimensionality of the current column.
     :  CURID    ! Current identifier.

*
*    Variables for each data type.

      BYTE             BVAL               ! Byte.
      INTEGER*2        WVAL               ! word.
      INTEGER          IVAL               ! Integer.
      REAL             RVAL               ! Real.
      DOUBLE PRECISION DVAL               ! Double precision.
      LOGICAL          LVAL               ! Logical.
      CHARACTER        CVAL*(CAT__SZVAL)  ! Character.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Extract the list of scalar and vector element field identifiers
*       corresponding to the table.

         NFIELD = 0

         DO LOOP = 1, NIDS__CAT1

*
*          First check the scalar columns.

            IF (IDPRN__CAT1(LOOP) .EQ. CI  .AND.
     :          IDTYP__CAT1(LOOP) .EQ. CAT__FITYP) THEN
               CALL CAT_TIQAI (LOOP, 'DIMS', DIM, STATUS)

               IF (DIM .EQ. CAT__SCALR) THEN
                  NFIELD = NFIELD + 1

                  FIA(NFIELD) = LOOP
                  POSN(NFIELD) = FPOSN__CAT5(LOOP)
               END IF

            ELSE IF (IDPRN__CAT1(LOOP) .EQ. CI  .AND.
     :                IDTYP__CAT1(LOOP) .EQ. CAT__FETYP) THEN
               NFIELD = NFIELD + 1

               FIA(NFIELD) = LOOP
               POSN(NFIELD) = FPOSN__CAT5(LOOP)
            END IF
         END DO

*
*       Assemble the list of field identifiers in order of their
*       position in the table.

         DO LOOP = 1, NFIELD
            MORE = .TRUE.
            CURID = 0

            DO WHILE (MORE)
               CURID = CURID + 1

               IF (POSN(CURID) .EQ. LOOP) THEN
                  FIAT(LOOP) = FIA(CURID)
                  MORE = .FALSE.
               END IF

               IF (CURID .GE. NFIELD) THEN
                  IF (MORE) THEN
                     MORE = .FALSE.
                     STATUS = CAT__INVID
                  END IF
               END IF
            END DO
         END DO

*
*       Proceed if all ok and at least one field was found.

         IF (STATUS .EQ. CAT__OK  .AND.  NFIELD .GT. 0) THEN

*
*          Process every row in the table.

            DO ROW = 1, ROWS
               BUFFER = ' '
               BUFLEN = -2

*
*             Process every field in the catalogue.

               DO CFIELD = 1, NFIELD
                  FIELD = FIAT(CFIELD)

                  BUFLEN = BUFLEN + 2

                  PTRN = FPTRN__CAT5(FIELD)

*
*                Check that the value is not null.

                  CALL CAT5_GTAEL (ROWS, ROW, %VAL(CNF_PVAL(PTRN)),
     :                             NULFLG,
     :              STATUS)
                  IF (.NOT. NULFLG) THEN

*
*                   Get the value for the field and write it to a
*                   character buffer.  First try to write the
*                   value using the external format for the field.
*                   If that fails then use a CHR routine.


                     FDTYPE = FDTYP__CAT5(FIELD)
                     FCSIZE = FCSIZ__CAT5(FIELD)
                     FFMT = FFMT__CAT5(FIELD)
                     PTR = FPTR__CAT5(FIELD)

                     IF (FDTYPE .EQ. CAT__TYPEB) THEN
                        CALL CAT5_GTAEB (ROWS, ROW, %VAL(CNF_PVAL(PTR)),
     :                                   BVAL,
     :                    STATUS)
                        IVAL = BVAL

                        WRITE(FLDBUF, '(' // FFMT // ')',
     :                    IOSTAT=LSTAT) IVAL

                        IF (LSTAT .NE. IOOK) THEN
                           FLDPOS = 0
                           FLDBUF = ' '

                           CALL CHR_PUTI (IVAL, FLDBUF, FLDPOS)
                        END IF

                     ELSE IF (FDTYPE .EQ. CAT__TYPEW) THEN
                        CALL CAT5_GTAEW (ROWS, ROW, %VAL(CNF_PVAL(PTR)),
     :                                   WVAL,
     :                    STATUS)
                        IVAL = WVAL

                        WRITE(FLDBUF, '(' // FFMT // ')',
     :                    IOSTAT=LSTAT) IVAL

                        IF (LSTAT .NE. IOOK) THEN
                           FLDPOS = 0
                           FLDBUF = ' '

                           CALL CHR_PUTI (IVAL, FLDBUF, FLDPOS)
                        END IF

                     ELSE IF (FDTYPE .EQ. CAT__TYPEI) THEN
                        CALL CAT5_GTAEI (ROWS, ROW, %VAL(CNF_PVAL(PTR)),
     :                                   IVAL,
     :                    STATUS)

                        WRITE(FLDBUF, '(' // FFMT // ')',
     :                    IOSTAT=LSTAT) IVAL

                        IF (LSTAT .NE. IOOK) THEN
                           FLDPOS = 0
                           FLDBUF = ' '

                           CALL CHR_PUTI (IVAL, FLDBUF, FLDPOS)
                        END IF

                     ELSE IF (FDTYPE .EQ. CAT__TYPER) THEN
                        CALL CAT5_GTAER (ROWS, ROW, %VAL(CNF_PVAL(PTR)),
     :                                   RVAL,
     :                    STATUS)

                        WRITE(FLDBUF, '(' // FFMT // ')',
     :                    IOSTAT=LSTAT) RVAL

                        IF (LSTAT .NE. IOOK) THEN
                           FLDPOS = 0
                           FLDBUF = ' '

                           CALL CHR_PUTR (RVAL, FLDBUF, FLDPOS)
                        END IF

                     ELSE IF (FDTYPE .EQ. CAT__TYPED) THEN
                        CALL CAT5_GTAED (ROWS, ROW, %VAL(CNF_PVAL(PTR)),
     :                                   DVAL,
     :                    STATUS)

                        WRITE(FLDBUF, '(' // FFMT // ')',
     :                    IOSTAT=LSTAT) DVAL

                        IF (LSTAT .NE. IOOK) THEN
                           FLDPOS = 0
                           FLDBUF = ' '

                           CALL CHR_PUTD (DVAL, FLDBUF, FLDPOS)
                        END IF

                     ELSE IF (FDTYPE .EQ. CAT__TYPEL) THEN
                        CALL CAT5_GTAEL (ROWS, ROW, %VAL(CNF_PVAL(PTR)),
     :                                   LVAL,
     :                    STATUS)

                        WRITE(FLDBUF, '(' // FFMT // ')',
     :                    IOSTAT=LSTAT) LVAL

                        IF (LSTAT .NE. IOOK) THEN
                           FLDPOS = 0
                           FLDBUF = ' '

                           CALL CHR_PUTL (LVAL, FLDBUF, FLDPOS)
                        END IF

                     ELSE IF (FDTYPE .EQ. CAT__TYPEC) THEN
                        CALL CAT5_GTCAE (ROWS, FCSIZE, ROW,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    CVAL, STATUS)

                        FLDPOS = 0
                        FLDBUF = ' '

                        CALL CHR_PUTC ('''', FLDBUF, FLDPOS)

                        IF (CVAL .NE. ' ') THEN
                           LCVAL = CHR_LEN(CVAL)
                           CALL CHR_PUTC (CVAL(1 : LCVAL), FLDBUF,
     :                       FLDPOS)
                        ELSE
                           FLDPOS = FLDPOS + 1
                        END IF

                        CALL CHR_PUTC ('''', FLDBUF, FLDPOS)

                     ELSE
                        STATUS = CAT__INVDT

                     END IF

                     IF (FLDBUF .EQ. ' ') THEN
                        FLDBUF = '<null>'
                     END IF

                     FLDPOS = CHR_LEN(FLDBUF)

*
*                   Append the string representing the field to the
*                   output buffer.

                     CALL CHR_PUTC (FLDBUF(1 : FLDPOS), BUFFER, BUFLEN)

                  ELSE

*
*                   The field is null; append the null indicator to
*                   the output buffer.

                     CALL CHR_PUTC ('<null>', BUFFER, BUFLEN)

                  END IF
               END DO

*
*             Attempt to write the record buffer to the file.

               WRITE(DFUNIT, 2000, IOSTAT=LSTAT) BUFFER(1 : BUFLEN)
 2000          FORMAT(A)
               IF (STATUS .EQ. CAT__OK) THEN
                  CALL CAT1_IOERR (LSTAT, STATUS)
                  IF (STATUS .NE. CAT__OK) THEN
                     ERRMSG = ' '
                     ERRLEN = 0

                     CALL CHR_PUTC ('Error writing record ', ERRMSG,
     :                 ERRLEN)
                     CALL CHR_PUTI (ROW, ERRMSG, ERRLEN)
                     CALL CHR_PUTC (' to the catalogue file.',
     :                 ERRMSG, ERRLEN)

                     CALL CAT1_ERREP ('CAT5_WRTBL_WRT',
     :                 ERRMSG(1 : ERRLEN), STATUS)
                  END IF
               END IF

            END DO

         ELSE


            IF (NFIELD .LE. 0) THEN

*
*             Failed to find any columns corresponding to the catalogue.

               STATUS = CAT__INVID

               CALL CAT1_ERREP ('CAT5_WRTBL_BDID',
     :           'Failed to find any column identifiers.', STATUS)
            ELSE
               CALL CAT1_ERREP ('CAT5_WRTBL_BDCL',
     :           'Error locating column identifiers.', STATUS)
            END IF

         END IF

      END IF

      END
