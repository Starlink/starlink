      SUBROUTINE CAT6_FIOB (IOFLG, CIELM, FI, ELEM, ROWNO, VALUE,
     :  NULFLG, STATUS)
*+
*  Name:
*     CAT6_FIOB
*  Purpose:
*     Get or put a value for a single field.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT6_FIOB (IOFLG, CIELM, FI, ELEM, ROWNO; VALUE, NULFLG;
*       STATUS)
*  Description:
*     Get or put a value for a single field.
*  Arguments:
*     IOFLG  =  LOGICAL (Given)
*        Flag indicating whether the routine is to get or put a value,
*        coded as follows:
*        .TRUE.  - get a value,
*        .FALSE. - put a value.
*        Not used in this ADC read-only version.
*     CIELM  =  INTEGER (Given)
*        Array element in the catalogue common blocks corresponding to
*        the catalogue to be accessed.
*     FI  =  INTEGER (Given)
*        Identifier to the field to be put or got.
*     ELEM  =  INTEGER (Given)
*        Element of the array which is to be put or got.
*     ROWNO  =  INTEGER (Given)
*        Number of the row in which a field is to be accessed.
*     VALUE  =  BYTE (Given and Returned)
*        Value to be put or obtained.
*     NULFLG  =  LOGICAL (Given and Returned)
*        Flag indicating whether or not the value is null.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Check whether the identifier corresponds to a scalar or a
*     vector.
*     If the identifier belongs to a scalar then
*       Copy the identifier
*     else (the identifier belongs to a vector)
*       Get the name of the column
*       Generate the name of the required vector element.
*       Get an identifier for this vector element.
*     end if
*     Attempt to obtain the pointers to the arrays holding the column
*     values and null flags.
*     If ok then
*       If getting a value then
*         Get the null value flag for the field.
*         If not null then
*           Get the actual value.
*         else
*           Adopt the appropriate null value.
*         end if
*       else (putting a value)
*         Set the value.
*         Set the null value flag.
*       end if
*     end if
*     Report any error.
*  Implementation Deficiencies:
*     Unsigned byte and unsigned word data types are not supported.
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
*     15/6/99 (ACD): Original version (from CAT5_FIOT.GEN).
*     15/6/99 (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT1_NUL'          ! CAT null values.
*  Global Variables:
      INCLUDE 'CAT1_CATS_CMN'     ! Catalogues common block.
      INCLUDE 'CAT1_IDS_CMN'      ! Identifiers common block.
      INCLUDE 'CAT6_TST_CMN'      ! TST back-end common block.
      INCLUDE 'CNF_PAR'           ! For CNF_PVAL function
*  Arguments Given:
      LOGICAL
     :  IOFLG
      INTEGER
     :  CIELM,
     :  FI,
     :  ELEM,
     :  ROWNO
*  Arguments Given and Returned:
      BYTE
     :  VALUE
      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS        ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      LOGICAL
     :  CONVOK    ! Flag; converted between data types ok?
      INTEGER
     :  FDIM,     ! Dimensionality of the column.
     :  FIC,      ! Identifier for the column or array element.
     :  LENAME,   ! Length of ENAME (excl. trail. blanks).
     :  FDTYPE,   ! Data type of the field.
     :  FCSIZE,   ! Size of the field if it is of type character.
     :  PTR,      ! Pointer to array of column values.
     :  PTRN,     ! Pointer to array of column null value flags.
     :  ROWS      ! Number of rows in the catalogue.
      CHARACTER
     :  ENAME*(CAT__SZCMP+4), ! Name of the required vector element.
     :  ERRMSG*75,            ! Text of error message.
     :  FNAME*(CAT__SZCMP)    ! Name of the current column.
      INTEGER
     :  ERRPOS,   ! Length of ERRMSG (excl. trail. blanks).
     :  LFNAME,   !   "    "  FNAME  ( "  .   "  .   "   ).
     :  LRSTAT    ! Local status, used when assembling error text.

*
*    'Input' variables for each data type.

      BYTE             UBVALI              ! Unsigned byte.
      BYTE             BVALI               ! Byte.
      INTEGER*2        UWVALI              ! Unsigned word.
      INTEGER*2        WVALI               ! word.
      INTEGER          IVALI               ! Integer.
      REAL             RVALI               ! Real.
      DOUBLE PRECISION DVALI               ! Double precision.
      LOGICAL          LVALI               ! Logical.
      CHARACTER        CVALI*(CAT__SZVAL)  ! Character.

*
*    'Output' variables for each data type.

      BYTE             UBVALO              ! Unsigned byte.
      BYTE             BVALO               ! Byte.
      INTEGER*2        UWVALO              ! Unsigned word.
      INTEGER*2        WVALO               ! word.
      INTEGER          IVALO               ! Integer.
      REAL             RVALO               ! Real.
      DOUBLE PRECISION DVALO               ! Double precision.
      LOGICAL          LVALO               ! Logical.
      CHARACTER        CVALO*(CAT__SZVAL)  ! Character.
*.

      IF (STATUS .EQ. CAT__OK) THEN

C       print4000, ioflg, cielm, fi, elem, rowno
C4000    format(1x, 'ioflg, cielm, fi, elem, rowno: ',
C    :     l5, i5, i5, i5, i5)

*
*       Check whether the identifier corresponds to a scalar or a
*       vector and then obtain the appropriate identifier.

         CALL CAT_TIQAI (FI, 'DIMS', FDIM, STATUS)

         IF (FDIM .EQ. CAT__SCALR) THEN
            FIC = FI

         ELSE
            CALL CAT_TIQAC (FI, 'NAME', FNAME, LRSTAT)

            ENAME = ' '
            LENAME = 0

            LFNAME = CHR_LEN(FNAME)
            CALL CHR_PUTC (FNAME(1 : LFNAME), ENAME, LENAME)
            CALL CHR_PUTC ('[', ENAME, LENAME)
            CALL CHR_PUTI (ELEM, ENAME, LENAME)
            CALL CHR_PUTC (']', ENAME, LENAME)

            CALL CAT_TIDNT(CIDS__CAT1(CIELM), ENAME(1 : LENAME),
     :        FIC, STATUS)
         END IF

C        print4100, fic
C4100    format(1x, 'fic: ', i5 )

*
*       Attempt to obtain the pointers to the arrays holding the column
*       values and null flags.

         IF (FIC .GT. 0  .AND.  FIC .LE. NIDS__CAT1) THEN
            FDTYPE = FDTYP__CAT6(FIC)
            FCSIZE = FCSIZ__CAT6(FIC)
            PTR = FPTR__CAT6(FIC)
            PTRN = FPTRN__CAT6(FIC)
         ELSE
            STATUS = CAT__INVID
         END IF

*
*       Proceed if all is ok.

         IF (STATUS .EQ. CAT__OK) THEN
            ROWS = EROW__CAT1(CIELM)
C           print4200, rows
C4200       format(1x, 'rows: ', i5)

*
*          Check whether the value is to be got or put.

            IF (IOFLG) THEN

*
*             A value is to be got.  First get the null value flag.
*             If the value is not null then get the actual datum,
*             otherwise substitute the appropriate null value.

               CALL CAT6_GTAEL (ROWS, ROWNO, %VAL(CNF_PVAL(PTRN)),
     :                          NULFLG, STATUS)

               IF (.NOT. NULFLG) THEN

                  IF (FDTYPE .EQ. CAT__TYPEB) THEN
                     CALL CAT6_GTAEB (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                BVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPEW) THEN
                     CALL CAT6_GTAEW (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                WVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPEI) THEN
                     CALL CAT6_GTAEI (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                IVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPER) THEN
                     CALL CAT6_GTAER (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                RVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPED) THEN
                     CALL CAT6_GTAED (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                DVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPEL) THEN
                     CALL CAT6_GTAEL (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                LVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPEC) THEN
                     CALL CAT6_GTCAE(ROWS, FCSIZE, ROWNO,
     :                               %VAL(CNF_PVAL(PTR)),
     :                 CVALI, STATUS)
                  ELSE
                     STATUS = CAT__INVDT
                  END IF

                  CALL CAT1_TCNVT (FDTYPE, UBVALI, BVALI, UWVALI,
     :              WVALI, IVALI, RVALI, DVALI, LVALI, CVALI,
     :              CAT__TYPEB, UBVALO, BVALO, UWVALO, WVALO,
     :              IVALO, RVALO, DVALO, LVALO, CVALO, CONVOK,
     :              STATUS)

                  IF (CONVOK) THEN
                     VALUE = BVALO
                  ELSE
                     NULFLG = .TRUE.
                     VALUE = CAT1__DNULB
                  END IF

               ELSE
                  VALUE = CAT1__DNULB
               END IF

            ELSE

*
*             A value is to be put.

               IF (.NOT. NULFLG) THEN
                  BVALI = VALUE

                  CALL CAT1_TCNVT (CAT__TYPEB, UBVALI, BVALI, UWVALI,
     :              WVALI, IVALI, RVALI, DVALI, LVALI, CVALI,
     :              FDTYPE, UBVALO, BVALO, UWVALO, WVALO,
     :              IVALO, RVALO, DVALO, LVALO, CVALO, CONVOK,
     :              STATUS)

                  IF (CONVOK) THEN
                     IF (FDTYPE .EQ. CAT__TYPEB) THEN
                        CALL CAT6_STAEB (ROWS, ROWNO, BVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPEW) THEN
                        CALL CAT6_STAEW (ROWS, ROWNO, WVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPEI) THEN
                        CALL CAT6_STAEI (ROWS, ROWNO, IVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPER) THEN
                        CALL CAT6_STAER (ROWS, ROWNO, RVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPED) THEN
                        CALL CAT6_STAED (ROWS, ROWNO, DVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPEL) THEN
                        CALL CAT6_STAEL (ROWS, ROWNO, LVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPEC) THEN
                        CALL CAT6_STCAE(ROWS, FCSIZE, ROWNO, CVALO,
     :                    %VAL(CNF_PVAL(PTR)), STATUS)
                     ELSE
                        STATUS = CAT__INVDT
                     END IF

                     CALL CAT6_STAEL (ROWS, ROWNO, .FALSE.,
     :                                %VAL(CNF_PVAL(PTRN)),
     :                 STATUS)
                  ELSE
                     CALL CAT6_STAEL (ROWS, ROWNO, .TRUE.,
     :                                %VAL(CNF_PVAL(PTRN)),
     :                 STATUS)
                  END IF
               ELSE
                  CALL CAT6_STAEL (ROWS, ROWNO, .TRUE.,
     :                             %VAL(CNF_PVAL(PTRN)),
     :              STATUS)
               END IF

            END IF
         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            ERRPOS = 0
            ERRMSG = ' '

            CALL CHR_PUTC ('Failure getting field from TST catalogue '/
     :        /'(column: ', ERRMSG, ERRPOS)

            LRSTAT = CAT__OK
            CALL CAT_TIQAC (FI, 'NAME', FNAME, LRSTAT)

            IF (LRSTAT .EQ. CAT__OK  .AND.  FNAME .NE. ' ') THEN
               LFNAME = CHR_LEN(FNAME)
               CALL CHR_PUTC (FNAME(1 : LFNAME), ERRMSG, ERRPOS)
            ELSE
               CALL CHR_PUTC ('<unknown>', ERRMSG, ERRPOS)
            END IF

            CALL CHR_PUTC (', element: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ELEM, ERRMSG, ERRPOS)
            CALL CHR_PUTC (', row: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ROWNO, ERRMSG, ERRPOS)
            CALL CHR_PUTC (').', ERRMSG, ERRPOS)

            CALL CAT1_ERREP ('CAT6_FIOB_ERR', ERRMSG(1 : ERRPOS),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT6_FIOC (IOFLG, CIELM, FI, ELEM, ROWNO, VALUE,
     :  NULFLG, STATUS)
*+
*  Name:
*     CAT6_FIOC
*  Purpose:
*     Get or put a value for a single field.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT6_FIOC (IOFLG, CIELM, FI, ELEM, ROWNO; VALUE, NULFLG;
*       STATUS)
*  Description:
*     Get or put a value for a single field.
*  Arguments:
*     IOFLG  =  LOGICAL (Given)
*        Flag indicating whether the routine is to get or put a value,
*        coded as follows:
*        .TRUE.  - get a value,
*        .FALSE. - put a value.
*        Not used in this ADC read-only version.
*     CIELM  =  INTEGER (Given)
*        Array element in the catalogue common blocks corresponding to
*        the catalogue to be accessed.
*     FI  =  INTEGER (Given)
*        Identifier to the field to be put or got.
*     ELEM  =  INTEGER (Given)
*        Element of the array which is to be put or got.
*     ROWNO  =  INTEGER (Given)
*        Number of the row in which a field is to be accessed.
*     VALUE  =  CHARACTER*(*) (Given and Returned)
*        Value to be put or obtained.
*     NULFLG  =  LOGICAL (Given and Returned)
*        Flag indicating whether or not the value is null.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Check whether the identifier corresponds to a scalar or a
*     vector.
*     If the identifier belongs to a scalar then
*       Copy the identifier
*     else (the identifier belongs to a vector)
*       Get the name of the column
*       Generate the name of the required vector element.
*       Get an identifier for this vector element.
*     end if
*     Attempt to obtain the pointers to the arrays holding the column
*     values and null flags.
*     If ok then
*       If getting a value then
*         Get the null value flag for the field.
*         If not null then
*           Get the actual value.
*         else
*           Adopt the appropriate null value.
*         end if
*       else (putting a value)
*         Set the value.
*         Set the null value flag.
*       end if
*     end if
*     Report any error.
*  Implementation Deficiencies:
*     Unsigned byte and unsigned word data types are not supported.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     15/6/99 (ACD): Original version (from CAT5_FIOT.GEN).
*     15/6/99 (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT1_NUL'          ! CAT null values.
*  Global Variables:
      INCLUDE 'CAT1_CATS_CMN'     ! Catalogues common block.
      INCLUDE 'CAT1_IDS_CMN'      ! Identifiers common block.
      INCLUDE 'CAT6_TST_CMN'      ! TST back-end common block.
      INCLUDE 'CNF_PAR'           ! For CNF_PVAL function
*  Arguments Given:
      LOGICAL
     :  IOFLG
      INTEGER
     :  CIELM,
     :  FI,
     :  ELEM,
     :  ROWNO
*  Arguments Given and Returned:
      CHARACTER*(*)
     :  VALUE
      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS        ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      LOGICAL
     :  CONVOK    ! Flag; converted between data types ok?
      INTEGER
     :  FDIM,     ! Dimensionality of the column.
     :  FIC,      ! Identifier for the column or array element.
     :  LENAME,   ! Length of ENAME (excl. trail. blanks).
     :  FDTYPE,   ! Data type of the field.
     :  FCSIZE,   ! Size of the field if it is of type character.
     :  PTR,      ! Pointer to array of column values.
     :  PTRN,     ! Pointer to array of column null value flags.
     :  ROWS      ! Number of rows in the catalogue.
      CHARACTER
     :  ENAME*(CAT__SZCMP+4), ! Name of the required vector element.
     :  ERRMSG*75,            ! Text of error message.
     :  FNAME*(CAT__SZCMP)    ! Name of the current column.
      INTEGER
     :  ERRPOS,   ! Length of ERRMSG (excl. trail. blanks).
     :  LFNAME,   !   "    "  FNAME  ( "  .   "  .   "   ).
     :  LRSTAT    ! Local status, used when assembling error text.

*
*    'Input' variables for each data type.

      BYTE             UBVALI              ! Unsigned byte.
      BYTE             BVALI               ! Byte.
      INTEGER*2        UWVALI              ! Unsigned word.
      INTEGER*2        WVALI               ! word.
      INTEGER          IVALI               ! Integer.
      REAL             RVALI               ! Real.
      DOUBLE PRECISION DVALI               ! Double precision.
      LOGICAL          LVALI               ! Logical.
      CHARACTER        CVALI*(CAT__SZVAL)  ! Character.

*
*    'Output' variables for each data type.

      BYTE             UBVALO              ! Unsigned byte.
      BYTE             BVALO               ! Byte.
      INTEGER*2        UWVALO              ! Unsigned word.
      INTEGER*2        WVALO               ! word.
      INTEGER          IVALO               ! Integer.
      REAL             RVALO               ! Real.
      DOUBLE PRECISION DVALO               ! Double precision.
      LOGICAL          LVALO               ! Logical.
      CHARACTER        CVALO*(CAT__SZVAL)  ! Character.
*.

      IF (STATUS .EQ. CAT__OK) THEN

C       print4000, ioflg, cielm, fi, elem, rowno
C4000    format(1x, 'ioflg, cielm, fi, elem, rowno: ',
C    :     l5, i5, i5, i5, i5)

*
*       Check whether the identifier corresponds to a scalar or a
*       vector and then obtain the appropriate identifier.

         CALL CAT_TIQAI (FI, 'DIMS', FDIM, STATUS)

         IF (FDIM .EQ. CAT__SCALR) THEN
            FIC = FI

         ELSE
            CALL CAT_TIQAC (FI, 'NAME', FNAME, LRSTAT)

            ENAME = ' '
            LENAME = 0

            LFNAME = CHR_LEN(FNAME)
            CALL CHR_PUTC (FNAME(1 : LFNAME), ENAME, LENAME)
            CALL CHR_PUTC ('[', ENAME, LENAME)
            CALL CHR_PUTI (ELEM, ENAME, LENAME)
            CALL CHR_PUTC (']', ENAME, LENAME)

            CALL CAT_TIDNT(CIDS__CAT1(CIELM), ENAME(1 : LENAME),
     :        FIC, STATUS)
         END IF

C        print4100, fic
C4100    format(1x, 'fic: ', i5 )

*
*       Attempt to obtain the pointers to the arrays holding the column
*       values and null flags.

         IF (FIC .GT. 0  .AND.  FIC .LE. NIDS__CAT1) THEN
            FDTYPE = FDTYP__CAT6(FIC)
            FCSIZE = FCSIZ__CAT6(FIC)
            PTR = FPTR__CAT6(FIC)
            PTRN = FPTRN__CAT6(FIC)
         ELSE
            STATUS = CAT__INVID
         END IF

*
*       Proceed if all is ok.

         IF (STATUS .EQ. CAT__OK) THEN
            ROWS = EROW__CAT1(CIELM)
C           print4200, rows
C4200       format(1x, 'rows: ', i5)

*
*          Check whether the value is to be got or put.

            IF (IOFLG) THEN

*
*             A value is to be got.  First get the null value flag.
*             If the value is not null then get the actual datum,
*             otherwise substitute the appropriate null value.

               CALL CAT6_GTAEL (ROWS, ROWNO, %VAL(CNF_PVAL(PTRN)),
     :                          NULFLG, STATUS)

               IF (.NOT. NULFLG) THEN

                  IF (FDTYPE .EQ. CAT__TYPEB) THEN
                     CALL CAT6_GTAEB (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                BVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPEW) THEN
                     CALL CAT6_GTAEW (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                WVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPEI) THEN
                     CALL CAT6_GTAEI (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                IVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPER) THEN
                     CALL CAT6_GTAER (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                RVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPED) THEN
                     CALL CAT6_GTAED (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                DVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPEL) THEN
                     CALL CAT6_GTAEL (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                LVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPEC) THEN
                     CALL CAT6_GTCAE(ROWS, FCSIZE, ROWNO,
     :                               %VAL(CNF_PVAL(PTR)),
     :                 CVALI, STATUS)
                  ELSE
                     STATUS = CAT__INVDT
                  END IF

                  CALL CAT1_TCNVT (FDTYPE, UBVALI, BVALI, UWVALI,
     :              WVALI, IVALI, RVALI, DVALI, LVALI, CVALI,
     :              CAT__TYPEC, UBVALO, BVALO, UWVALO, WVALO,
     :              IVALO, RVALO, DVALO, LVALO, CVALO, CONVOK,
     :              STATUS)

                  IF (CONVOK) THEN
                     VALUE = CVALO
                  ELSE
                     NULFLG = .TRUE.
                     VALUE = CAT1__DNULC
                  END IF

               ELSE
                  VALUE = CAT1__DNULC
               END IF

            ELSE

*
*             A value is to be put.

               IF (.NOT. NULFLG) THEN
                  CVALI = VALUE

                  CALL CAT1_TCNVT (CAT__TYPEC, UBVALI, BVALI, UWVALI,
     :              WVALI, IVALI, RVALI, DVALI, LVALI, CVALI,
     :              FDTYPE, UBVALO, BVALO, UWVALO, WVALO,
     :              IVALO, RVALO, DVALO, LVALO, CVALO, CONVOK,
     :              STATUS)

                  IF (CONVOK) THEN
                     IF (FDTYPE .EQ. CAT__TYPEB) THEN
                        CALL CAT6_STAEB (ROWS, ROWNO, BVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPEW) THEN
                        CALL CAT6_STAEW (ROWS, ROWNO, WVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPEI) THEN
                        CALL CAT6_STAEI (ROWS, ROWNO, IVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPER) THEN
                        CALL CAT6_STAER (ROWS, ROWNO, RVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPED) THEN
                        CALL CAT6_STAED (ROWS, ROWNO, DVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPEL) THEN
                        CALL CAT6_STAEL (ROWS, ROWNO, LVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPEC) THEN
                        CALL CAT6_STCAE(ROWS, FCSIZE, ROWNO, CVALO,
     :                    %VAL(CNF_PVAL(PTR)), STATUS)
                     ELSE
                        STATUS = CAT__INVDT
                     END IF

                     CALL CAT6_STAEL (ROWS, ROWNO, .FALSE.,
     :                                %VAL(CNF_PVAL(PTRN)),
     :                 STATUS)
                  ELSE
                     CALL CAT6_STAEL (ROWS, ROWNO, .TRUE.,
     :                                %VAL(CNF_PVAL(PTRN)),
     :                 STATUS)
                  END IF
               ELSE
                  CALL CAT6_STAEL (ROWS, ROWNO, .TRUE.,
     :                             %VAL(CNF_PVAL(PTRN)),
     :              STATUS)
               END IF

            END IF
         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            ERRPOS = 0
            ERRMSG = ' '

            CALL CHR_PUTC ('Failure getting field from TST catalogue '/
     :        /'(column: ', ERRMSG, ERRPOS)

            LRSTAT = CAT__OK
            CALL CAT_TIQAC (FI, 'NAME', FNAME, LRSTAT)

            IF (LRSTAT .EQ. CAT__OK  .AND.  FNAME .NE. ' ') THEN
               LFNAME = CHR_LEN(FNAME)
               CALL CHR_PUTC (FNAME(1 : LFNAME), ERRMSG, ERRPOS)
            ELSE
               CALL CHR_PUTC ('<unknown>', ERRMSG, ERRPOS)
            END IF

            CALL CHR_PUTC (', element: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ELEM, ERRMSG, ERRPOS)
            CALL CHR_PUTC (', row: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ROWNO, ERRMSG, ERRPOS)
            CALL CHR_PUTC (').', ERRMSG, ERRPOS)

            CALL CAT1_ERREP ('CAT6_FIOC_ERR', ERRMSG(1 : ERRPOS),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT6_FIOD (IOFLG, CIELM, FI, ELEM, ROWNO, VALUE,
     :  NULFLG, STATUS)
*+
*  Name:
*     CAT6_FIOD
*  Purpose:
*     Get or put a value for a single field.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT6_FIOD (IOFLG, CIELM, FI, ELEM, ROWNO; VALUE, NULFLG;
*       STATUS)
*  Description:
*     Get or put a value for a single field.
*  Arguments:
*     IOFLG  =  LOGICAL (Given)
*        Flag indicating whether the routine is to get or put a value,
*        coded as follows:
*        .TRUE.  - get a value,
*        .FALSE. - put a value.
*        Not used in this ADC read-only version.
*     CIELM  =  INTEGER (Given)
*        Array element in the catalogue common blocks corresponding to
*        the catalogue to be accessed.
*     FI  =  INTEGER (Given)
*        Identifier to the field to be put or got.
*     ELEM  =  INTEGER (Given)
*        Element of the array which is to be put or got.
*     ROWNO  =  INTEGER (Given)
*        Number of the row in which a field is to be accessed.
*     VALUE  =  DOUBLE PRECISION (Given and Returned)
*        Value to be put or obtained.
*     NULFLG  =  LOGICAL (Given and Returned)
*        Flag indicating whether or not the value is null.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Check whether the identifier corresponds to a scalar or a
*     vector.
*     If the identifier belongs to a scalar then
*       Copy the identifier
*     else (the identifier belongs to a vector)
*       Get the name of the column
*       Generate the name of the required vector element.
*       Get an identifier for this vector element.
*     end if
*     Attempt to obtain the pointers to the arrays holding the column
*     values and null flags.
*     If ok then
*       If getting a value then
*         Get the null value flag for the field.
*         If not null then
*           Get the actual value.
*         else
*           Adopt the appropriate null value.
*         end if
*       else (putting a value)
*         Set the value.
*         Set the null value flag.
*       end if
*     end if
*     Report any error.
*  Implementation Deficiencies:
*     Unsigned byte and unsigned word data types are not supported.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     15/6/99 (ACD): Original version (from CAT5_FIOT.GEN).
*     15/6/99 (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT1_NUL'          ! CAT null values.
*  Global Variables:
      INCLUDE 'CAT1_CATS_CMN'     ! Catalogues common block.
      INCLUDE 'CAT1_IDS_CMN'      ! Identifiers common block.
      INCLUDE 'CAT6_TST_CMN'      ! TST back-end common block.
      INCLUDE 'CNF_PAR'           ! For CNF_PVAL function
*  Arguments Given:
      LOGICAL
     :  IOFLG
      INTEGER
     :  CIELM,
     :  FI,
     :  ELEM,
     :  ROWNO
*  Arguments Given and Returned:
      DOUBLE PRECISION
     :  VALUE
      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS        ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      LOGICAL
     :  CONVOK    ! Flag; converted between data types ok?
      INTEGER
     :  FDIM,     ! Dimensionality of the column.
     :  FIC,      ! Identifier for the column or array element.
     :  LENAME,   ! Length of ENAME (excl. trail. blanks).
     :  FDTYPE,   ! Data type of the field.
     :  FCSIZE,   ! Size of the field if it is of type character.
     :  PTR,      ! Pointer to array of column values.
     :  PTRN,     ! Pointer to array of column null value flags.
     :  ROWS      ! Number of rows in the catalogue.
      CHARACTER
     :  ENAME*(CAT__SZCMP+4), ! Name of the required vector element.
     :  ERRMSG*75,            ! Text of error message.
     :  FNAME*(CAT__SZCMP)    ! Name of the current column.
      INTEGER
     :  ERRPOS,   ! Length of ERRMSG (excl. trail. blanks).
     :  LFNAME,   !   "    "  FNAME  ( "  .   "  .   "   ).
     :  LRSTAT    ! Local status, used when assembling error text.

*
*    'Input' variables for each data type.

      BYTE             UBVALI              ! Unsigned byte.
      BYTE             BVALI               ! Byte.
      INTEGER*2        UWVALI              ! Unsigned word.
      INTEGER*2        WVALI               ! word.
      INTEGER          IVALI               ! Integer.
      REAL             RVALI               ! Real.
      DOUBLE PRECISION DVALI               ! Double precision.
      LOGICAL          LVALI               ! Logical.
      CHARACTER        CVALI*(CAT__SZVAL)  ! Character.

*
*    'Output' variables for each data type.

      BYTE             UBVALO              ! Unsigned byte.
      BYTE             BVALO               ! Byte.
      INTEGER*2        UWVALO              ! Unsigned word.
      INTEGER*2        WVALO               ! word.
      INTEGER          IVALO               ! Integer.
      REAL             RVALO               ! Real.
      DOUBLE PRECISION DVALO               ! Double precision.
      LOGICAL          LVALO               ! Logical.
      CHARACTER        CVALO*(CAT__SZVAL)  ! Character.
*.

      IF (STATUS .EQ. CAT__OK) THEN

C       print4000, ioflg, cielm, fi, elem, rowno
C4000    format(1x, 'ioflg, cielm, fi, elem, rowno: ',
C    :     l5, i5, i5, i5, i5)

*
*       Check whether the identifier corresponds to a scalar or a
*       vector and then obtain the appropriate identifier.

         CALL CAT_TIQAI (FI, 'DIMS', FDIM, STATUS)

         IF (FDIM .EQ. CAT__SCALR) THEN
            FIC = FI

         ELSE
            CALL CAT_TIQAC (FI, 'NAME', FNAME, LRSTAT)

            ENAME = ' '
            LENAME = 0

            LFNAME = CHR_LEN(FNAME)
            CALL CHR_PUTC (FNAME(1 : LFNAME), ENAME, LENAME)
            CALL CHR_PUTC ('[', ENAME, LENAME)
            CALL CHR_PUTI (ELEM, ENAME, LENAME)
            CALL CHR_PUTC (']', ENAME, LENAME)

            CALL CAT_TIDNT(CIDS__CAT1(CIELM), ENAME(1 : LENAME),
     :        FIC, STATUS)
         END IF

C        print4100, fic
C4100    format(1x, 'fic: ', i5 )

*
*       Attempt to obtain the pointers to the arrays holding the column
*       values and null flags.

         IF (FIC .GT. 0  .AND.  FIC .LE. NIDS__CAT1) THEN
            FDTYPE = FDTYP__CAT6(FIC)
            FCSIZE = FCSIZ__CAT6(FIC)
            PTR = FPTR__CAT6(FIC)
            PTRN = FPTRN__CAT6(FIC)
         ELSE
            STATUS = CAT__INVID
         END IF

*
*       Proceed if all is ok.

         IF (STATUS .EQ. CAT__OK) THEN
            ROWS = EROW__CAT1(CIELM)
C           print4200, rows
C4200       format(1x, 'rows: ', i5)

*
*          Check whether the value is to be got or put.

            IF (IOFLG) THEN

*
*             A value is to be got.  First get the null value flag.
*             If the value is not null then get the actual datum,
*             otherwise substitute the appropriate null value.

               CALL CAT6_GTAEL (ROWS, ROWNO, %VAL(CNF_PVAL(PTRN)),
     :                          NULFLG, STATUS)

               IF (.NOT. NULFLG) THEN

                  IF (FDTYPE .EQ. CAT__TYPEB) THEN
                     CALL CAT6_GTAEB (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                BVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPEW) THEN
                     CALL CAT6_GTAEW (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                WVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPEI) THEN
                     CALL CAT6_GTAEI (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                IVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPER) THEN
                     CALL CAT6_GTAER (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                RVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPED) THEN
                     CALL CAT6_GTAED (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                DVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPEL) THEN
                     CALL CAT6_GTAEL (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                LVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPEC) THEN
                     CALL CAT6_GTCAE(ROWS, FCSIZE, ROWNO,
     :                               %VAL(CNF_PVAL(PTR)),
     :                 CVALI, STATUS)
                  ELSE
                     STATUS = CAT__INVDT
                  END IF

                  CALL CAT1_TCNVT (FDTYPE, UBVALI, BVALI, UWVALI,
     :              WVALI, IVALI, RVALI, DVALI, LVALI, CVALI,
     :              CAT__TYPED, UBVALO, BVALO, UWVALO, WVALO,
     :              IVALO, RVALO, DVALO, LVALO, CVALO, CONVOK,
     :              STATUS)

                  IF (CONVOK) THEN
                     VALUE = DVALO
                  ELSE
                     NULFLG = .TRUE.
                     VALUE = CAT1__DNULD
                  END IF

               ELSE
                  VALUE = CAT1__DNULD
               END IF

            ELSE

*
*             A value is to be put.

               IF (.NOT. NULFLG) THEN
                  DVALI = VALUE

                  CALL CAT1_TCNVT (CAT__TYPED, UBVALI, BVALI, UWVALI,
     :              WVALI, IVALI, RVALI, DVALI, LVALI, CVALI,
     :              FDTYPE, UBVALO, BVALO, UWVALO, WVALO,
     :              IVALO, RVALO, DVALO, LVALO, CVALO, CONVOK,
     :              STATUS)

                  IF (CONVOK) THEN
                     IF (FDTYPE .EQ. CAT__TYPEB) THEN
                        CALL CAT6_STAEB (ROWS, ROWNO, BVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPEW) THEN
                        CALL CAT6_STAEW (ROWS, ROWNO, WVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPEI) THEN
                        CALL CAT6_STAEI (ROWS, ROWNO, IVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPER) THEN
                        CALL CAT6_STAER (ROWS, ROWNO, RVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPED) THEN
                        CALL CAT6_STAED (ROWS, ROWNO, DVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPEL) THEN
                        CALL CAT6_STAEL (ROWS, ROWNO, LVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPEC) THEN
                        CALL CAT6_STCAE(ROWS, FCSIZE, ROWNO, CVALO,
     :                    %VAL(CNF_PVAL(PTR)), STATUS)
                     ELSE
                        STATUS = CAT__INVDT
                     END IF

                     CALL CAT6_STAEL (ROWS, ROWNO, .FALSE.,
     :                                %VAL(CNF_PVAL(PTRN)),
     :                 STATUS)
                  ELSE
                     CALL CAT6_STAEL (ROWS, ROWNO, .TRUE.,
     :                                %VAL(CNF_PVAL(PTRN)),
     :                 STATUS)
                  END IF
               ELSE
                  CALL CAT6_STAEL (ROWS, ROWNO, .TRUE.,
     :                             %VAL(CNF_PVAL(PTRN)),
     :              STATUS)
               END IF

            END IF
         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            ERRPOS = 0
            ERRMSG = ' '

            CALL CHR_PUTC ('Failure getting field from TST catalogue '/
     :        /'(column: ', ERRMSG, ERRPOS)

            LRSTAT = CAT__OK
            CALL CAT_TIQAC (FI, 'NAME', FNAME, LRSTAT)

            IF (LRSTAT .EQ. CAT__OK  .AND.  FNAME .NE. ' ') THEN
               LFNAME = CHR_LEN(FNAME)
               CALL CHR_PUTC (FNAME(1 : LFNAME), ERRMSG, ERRPOS)
            ELSE
               CALL CHR_PUTC ('<unknown>', ERRMSG, ERRPOS)
            END IF

            CALL CHR_PUTC (', element: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ELEM, ERRMSG, ERRPOS)
            CALL CHR_PUTC (', row: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ROWNO, ERRMSG, ERRPOS)
            CALL CHR_PUTC (').', ERRMSG, ERRPOS)

            CALL CAT1_ERREP ('CAT6_FIOD_ERR', ERRMSG(1 : ERRPOS),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT6_FIOI (IOFLG, CIELM, FI, ELEM, ROWNO, VALUE,
     :  NULFLG, STATUS)
*+
*  Name:
*     CAT6_FIOI
*  Purpose:
*     Get or put a value for a single field.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT6_FIOI (IOFLG, CIELM, FI, ELEM, ROWNO; VALUE, NULFLG;
*       STATUS)
*  Description:
*     Get or put a value for a single field.
*  Arguments:
*     IOFLG  =  LOGICAL (Given)
*        Flag indicating whether the routine is to get or put a value,
*        coded as follows:
*        .TRUE.  - get a value,
*        .FALSE. - put a value.
*        Not used in this ADC read-only version.
*     CIELM  =  INTEGER (Given)
*        Array element in the catalogue common blocks corresponding to
*        the catalogue to be accessed.
*     FI  =  INTEGER (Given)
*        Identifier to the field to be put or got.
*     ELEM  =  INTEGER (Given)
*        Element of the array which is to be put or got.
*     ROWNO  =  INTEGER (Given)
*        Number of the row in which a field is to be accessed.
*     VALUE  =  INTEGER (Given and Returned)
*        Value to be put or obtained.
*     NULFLG  =  LOGICAL (Given and Returned)
*        Flag indicating whether or not the value is null.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Check whether the identifier corresponds to a scalar or a
*     vector.
*     If the identifier belongs to a scalar then
*       Copy the identifier
*     else (the identifier belongs to a vector)
*       Get the name of the column
*       Generate the name of the required vector element.
*       Get an identifier for this vector element.
*     end if
*     Attempt to obtain the pointers to the arrays holding the column
*     values and null flags.
*     If ok then
*       If getting a value then
*         Get the null value flag for the field.
*         If not null then
*           Get the actual value.
*         else
*           Adopt the appropriate null value.
*         end if
*       else (putting a value)
*         Set the value.
*         Set the null value flag.
*       end if
*     end if
*     Report any error.
*  Implementation Deficiencies:
*     Unsigned byte and unsigned word data types are not supported.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     15/6/99 (ACD): Original version (from CAT5_FIOT.GEN).
*     15/6/99 (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT1_NUL'          ! CAT null values.
*  Global Variables:
      INCLUDE 'CAT1_CATS_CMN'     ! Catalogues common block.
      INCLUDE 'CAT1_IDS_CMN'      ! Identifiers common block.
      INCLUDE 'CAT6_TST_CMN'      ! TST back-end common block.
      INCLUDE 'CNF_PAR'           ! For CNF_PVAL function
*  Arguments Given:
      LOGICAL
     :  IOFLG
      INTEGER
     :  CIELM,
     :  FI,
     :  ELEM,
     :  ROWNO
*  Arguments Given and Returned:
      INTEGER
     :  VALUE
      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS        ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      LOGICAL
     :  CONVOK    ! Flag; converted between data types ok?
      INTEGER
     :  FDIM,     ! Dimensionality of the column.
     :  FIC,      ! Identifier for the column or array element.
     :  LENAME,   ! Length of ENAME (excl. trail. blanks).
     :  FDTYPE,   ! Data type of the field.
     :  FCSIZE,   ! Size of the field if it is of type character.
     :  PTR,      ! Pointer to array of column values.
     :  PTRN,     ! Pointer to array of column null value flags.
     :  ROWS      ! Number of rows in the catalogue.
      CHARACTER
     :  ENAME*(CAT__SZCMP+4), ! Name of the required vector element.
     :  ERRMSG*75,            ! Text of error message.
     :  FNAME*(CAT__SZCMP)    ! Name of the current column.
      INTEGER
     :  ERRPOS,   ! Length of ERRMSG (excl. trail. blanks).
     :  LFNAME,   !   "    "  FNAME  ( "  .   "  .   "   ).
     :  LRSTAT    ! Local status, used when assembling error text.

*
*    'Input' variables for each data type.

      BYTE             UBVALI              ! Unsigned byte.
      BYTE             BVALI               ! Byte.
      INTEGER*2        UWVALI              ! Unsigned word.
      INTEGER*2        WVALI               ! word.
      INTEGER          IVALI               ! Integer.
      REAL             RVALI               ! Real.
      DOUBLE PRECISION DVALI               ! Double precision.
      LOGICAL          LVALI               ! Logical.
      CHARACTER        CVALI*(CAT__SZVAL)  ! Character.

*
*    'Output' variables for each data type.

      BYTE             UBVALO              ! Unsigned byte.
      BYTE             BVALO               ! Byte.
      INTEGER*2        UWVALO              ! Unsigned word.
      INTEGER*2        WVALO               ! word.
      INTEGER          IVALO               ! Integer.
      REAL             RVALO               ! Real.
      DOUBLE PRECISION DVALO               ! Double precision.
      LOGICAL          LVALO               ! Logical.
      CHARACTER        CVALO*(CAT__SZVAL)  ! Character.
*.

      IF (STATUS .EQ. CAT__OK) THEN

C       print4000, ioflg, cielm, fi, elem, rowno
C4000    format(1x, 'ioflg, cielm, fi, elem, rowno: ',
C    :     l5, i5, i5, i5, i5)

*
*       Check whether the identifier corresponds to a scalar or a
*       vector and then obtain the appropriate identifier.

         CALL CAT_TIQAI (FI, 'DIMS', FDIM, STATUS)

         IF (FDIM .EQ. CAT__SCALR) THEN
            FIC = FI

         ELSE
            CALL CAT_TIQAC (FI, 'NAME', FNAME, LRSTAT)

            ENAME = ' '
            LENAME = 0

            LFNAME = CHR_LEN(FNAME)
            CALL CHR_PUTC (FNAME(1 : LFNAME), ENAME, LENAME)
            CALL CHR_PUTC ('[', ENAME, LENAME)
            CALL CHR_PUTI (ELEM, ENAME, LENAME)
            CALL CHR_PUTC (']', ENAME, LENAME)

            CALL CAT_TIDNT(CIDS__CAT1(CIELM), ENAME(1 : LENAME),
     :        FIC, STATUS)
         END IF

C        print4100, fic
C4100    format(1x, 'fic: ', i5 )

*
*       Attempt to obtain the pointers to the arrays holding the column
*       values and null flags.

         IF (FIC .GT. 0  .AND.  FIC .LE. NIDS__CAT1) THEN
            FDTYPE = FDTYP__CAT6(FIC)
            FCSIZE = FCSIZ__CAT6(FIC)
            PTR = FPTR__CAT6(FIC)
            PTRN = FPTRN__CAT6(FIC)
         ELSE
            STATUS = CAT__INVID
         END IF

*
*       Proceed if all is ok.

         IF (STATUS .EQ. CAT__OK) THEN
            ROWS = EROW__CAT1(CIELM)
C           print4200, rows
C4200       format(1x, 'rows: ', i5)

*
*          Check whether the value is to be got or put.

            IF (IOFLG) THEN

*
*             A value is to be got.  First get the null value flag.
*             If the value is not null then get the actual datum,
*             otherwise substitute the appropriate null value.

               CALL CAT6_GTAEL (ROWS, ROWNO, %VAL(CNF_PVAL(PTRN)),
     :                          NULFLG, STATUS)

               IF (.NOT. NULFLG) THEN

                  IF (FDTYPE .EQ. CAT__TYPEB) THEN
                     CALL CAT6_GTAEB (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                BVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPEW) THEN
                     CALL CAT6_GTAEW (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                WVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPEI) THEN
                     CALL CAT6_GTAEI (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                IVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPER) THEN
                     CALL CAT6_GTAER (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                RVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPED) THEN
                     CALL CAT6_GTAED (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                DVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPEL) THEN
                     CALL CAT6_GTAEL (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                LVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPEC) THEN
                     CALL CAT6_GTCAE(ROWS, FCSIZE, ROWNO,
     :                               %VAL(CNF_PVAL(PTR)),
     :                 CVALI, STATUS)
                  ELSE
                     STATUS = CAT__INVDT
                  END IF

                  CALL CAT1_TCNVT (FDTYPE, UBVALI, BVALI, UWVALI,
     :              WVALI, IVALI, RVALI, DVALI, LVALI, CVALI,
     :              CAT__TYPEI, UBVALO, BVALO, UWVALO, WVALO,
     :              IVALO, RVALO, DVALO, LVALO, CVALO, CONVOK,
     :              STATUS)

                  IF (CONVOK) THEN
                     VALUE = IVALO
                  ELSE
                     NULFLG = .TRUE.
                     VALUE = CAT1__DNULI
                  END IF

               ELSE
                  VALUE = CAT1__DNULI
               END IF

            ELSE

*
*             A value is to be put.

               IF (.NOT. NULFLG) THEN
                  IVALI = VALUE

                  CALL CAT1_TCNVT (CAT__TYPEI, UBVALI, BVALI, UWVALI,
     :              WVALI, IVALI, RVALI, DVALI, LVALI, CVALI,
     :              FDTYPE, UBVALO, BVALO, UWVALO, WVALO,
     :              IVALO, RVALO, DVALO, LVALO, CVALO, CONVOK,
     :              STATUS)

                  IF (CONVOK) THEN
                     IF (FDTYPE .EQ. CAT__TYPEB) THEN
                        CALL CAT6_STAEB (ROWS, ROWNO, BVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPEW) THEN
                        CALL CAT6_STAEW (ROWS, ROWNO, WVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPEI) THEN
                        CALL CAT6_STAEI (ROWS, ROWNO, IVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPER) THEN
                        CALL CAT6_STAER (ROWS, ROWNO, RVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPED) THEN
                        CALL CAT6_STAED (ROWS, ROWNO, DVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPEL) THEN
                        CALL CAT6_STAEL (ROWS, ROWNO, LVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPEC) THEN
                        CALL CAT6_STCAE(ROWS, FCSIZE, ROWNO, CVALO,
     :                    %VAL(CNF_PVAL(PTR)), STATUS)
                     ELSE
                        STATUS = CAT__INVDT
                     END IF

                     CALL CAT6_STAEL (ROWS, ROWNO, .FALSE.,
     :                                %VAL(CNF_PVAL(PTRN)),
     :                 STATUS)
                  ELSE
                     CALL CAT6_STAEL (ROWS, ROWNO, .TRUE.,
     :                                %VAL(CNF_PVAL(PTRN)),
     :                 STATUS)
                  END IF
               ELSE
                  CALL CAT6_STAEL (ROWS, ROWNO, .TRUE.,
     :                             %VAL(CNF_PVAL(PTRN)),
     :              STATUS)
               END IF

            END IF
         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            ERRPOS = 0
            ERRMSG = ' '

            CALL CHR_PUTC ('Failure getting field from TST catalogue '/
     :        /'(column: ', ERRMSG, ERRPOS)

            LRSTAT = CAT__OK
            CALL CAT_TIQAC (FI, 'NAME', FNAME, LRSTAT)

            IF (LRSTAT .EQ. CAT__OK  .AND.  FNAME .NE. ' ') THEN
               LFNAME = CHR_LEN(FNAME)
               CALL CHR_PUTC (FNAME(1 : LFNAME), ERRMSG, ERRPOS)
            ELSE
               CALL CHR_PUTC ('<unknown>', ERRMSG, ERRPOS)
            END IF

            CALL CHR_PUTC (', element: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ELEM, ERRMSG, ERRPOS)
            CALL CHR_PUTC (', row: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ROWNO, ERRMSG, ERRPOS)
            CALL CHR_PUTC (').', ERRMSG, ERRPOS)

            CALL CAT1_ERREP ('CAT6_FIOI_ERR', ERRMSG(1 : ERRPOS),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT6_FIOL (IOFLG, CIELM, FI, ELEM, ROWNO, VALUE,
     :  NULFLG, STATUS)
*+
*  Name:
*     CAT6_FIOL
*  Purpose:
*     Get or put a value for a single field.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT6_FIOL (IOFLG, CIELM, FI, ELEM, ROWNO; VALUE, NULFLG;
*       STATUS)
*  Description:
*     Get or put a value for a single field.
*  Arguments:
*     IOFLG  =  LOGICAL (Given)
*        Flag indicating whether the routine is to get or put a value,
*        coded as follows:
*        .TRUE.  - get a value,
*        .FALSE. - put a value.
*        Not used in this ADC read-only version.
*     CIELM  =  INTEGER (Given)
*        Array element in the catalogue common blocks corresponding to
*        the catalogue to be accessed.
*     FI  =  INTEGER (Given)
*        Identifier to the field to be put or got.
*     ELEM  =  INTEGER (Given)
*        Element of the array which is to be put or got.
*     ROWNO  =  INTEGER (Given)
*        Number of the row in which a field is to be accessed.
*     VALUE  =  LOGICAL (Given and Returned)
*        Value to be put or obtained.
*     NULFLG  =  LOGICAL (Given and Returned)
*        Flag indicating whether or not the value is null.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Check whether the identifier corresponds to a scalar or a
*     vector.
*     If the identifier belongs to a scalar then
*       Copy the identifier
*     else (the identifier belongs to a vector)
*       Get the name of the column
*       Generate the name of the required vector element.
*       Get an identifier for this vector element.
*     end if
*     Attempt to obtain the pointers to the arrays holding the column
*     values and null flags.
*     If ok then
*       If getting a value then
*         Get the null value flag for the field.
*         If not null then
*           Get the actual value.
*         else
*           Adopt the appropriate null value.
*         end if
*       else (putting a value)
*         Set the value.
*         Set the null value flag.
*       end if
*     end if
*     Report any error.
*  Implementation Deficiencies:
*     Unsigned byte and unsigned word data types are not supported.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     15/6/99 (ACD): Original version (from CAT5_FIOT.GEN).
*     15/6/99 (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT1_NUL'          ! CAT null values.
*  Global Variables:
      INCLUDE 'CAT1_CATS_CMN'     ! Catalogues common block.
      INCLUDE 'CAT1_IDS_CMN'      ! Identifiers common block.
      INCLUDE 'CAT6_TST_CMN'      ! TST back-end common block.
      INCLUDE 'CNF_PAR'           ! For CNF_PVAL function
*  Arguments Given:
      LOGICAL
     :  IOFLG
      INTEGER
     :  CIELM,
     :  FI,
     :  ELEM,
     :  ROWNO
*  Arguments Given and Returned:
      LOGICAL
     :  VALUE
      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS        ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      LOGICAL
     :  CONVOK    ! Flag; converted between data types ok?
      INTEGER
     :  FDIM,     ! Dimensionality of the column.
     :  FIC,      ! Identifier for the column or array element.
     :  LENAME,   ! Length of ENAME (excl. trail. blanks).
     :  FDTYPE,   ! Data type of the field.
     :  FCSIZE,   ! Size of the field if it is of type character.
     :  PTR,      ! Pointer to array of column values.
     :  PTRN,     ! Pointer to array of column null value flags.
     :  ROWS      ! Number of rows in the catalogue.
      CHARACTER
     :  ENAME*(CAT__SZCMP+4), ! Name of the required vector element.
     :  ERRMSG*75,            ! Text of error message.
     :  FNAME*(CAT__SZCMP)    ! Name of the current column.
      INTEGER
     :  ERRPOS,   ! Length of ERRMSG (excl. trail. blanks).
     :  LFNAME,   !   "    "  FNAME  ( "  .   "  .   "   ).
     :  LRSTAT    ! Local status, used when assembling error text.

*
*    'Input' variables for each data type.

      BYTE             UBVALI              ! Unsigned byte.
      BYTE             BVALI               ! Byte.
      INTEGER*2        UWVALI              ! Unsigned word.
      INTEGER*2        WVALI               ! word.
      INTEGER          IVALI               ! Integer.
      REAL             RVALI               ! Real.
      DOUBLE PRECISION DVALI               ! Double precision.
      LOGICAL          LVALI               ! Logical.
      CHARACTER        CVALI*(CAT__SZVAL)  ! Character.

*
*    'Output' variables for each data type.

      BYTE             UBVALO              ! Unsigned byte.
      BYTE             BVALO               ! Byte.
      INTEGER*2        UWVALO              ! Unsigned word.
      INTEGER*2        WVALO               ! word.
      INTEGER          IVALO               ! Integer.
      REAL             RVALO               ! Real.
      DOUBLE PRECISION DVALO               ! Double precision.
      LOGICAL          LVALO               ! Logical.
      CHARACTER        CVALO*(CAT__SZVAL)  ! Character.
*.

      IF (STATUS .EQ. CAT__OK) THEN

C       print4000, ioflg, cielm, fi, elem, rowno
C4000    format(1x, 'ioflg, cielm, fi, elem, rowno: ',
C    :     l5, i5, i5, i5, i5)

*
*       Check whether the identifier corresponds to a scalar or a
*       vector and then obtain the appropriate identifier.

         CALL CAT_TIQAI (FI, 'DIMS', FDIM, STATUS)

         IF (FDIM .EQ. CAT__SCALR) THEN
            FIC = FI

         ELSE
            CALL CAT_TIQAC (FI, 'NAME', FNAME, LRSTAT)

            ENAME = ' '
            LENAME = 0

            LFNAME = CHR_LEN(FNAME)
            CALL CHR_PUTC (FNAME(1 : LFNAME), ENAME, LENAME)
            CALL CHR_PUTC ('[', ENAME, LENAME)
            CALL CHR_PUTI (ELEM, ENAME, LENAME)
            CALL CHR_PUTC (']', ENAME, LENAME)

            CALL CAT_TIDNT(CIDS__CAT1(CIELM), ENAME(1 : LENAME),
     :        FIC, STATUS)
         END IF

C        print4100, fic
C4100    format(1x, 'fic: ', i5 )

*
*       Attempt to obtain the pointers to the arrays holding the column
*       values and null flags.

         IF (FIC .GT. 0  .AND.  FIC .LE. NIDS__CAT1) THEN
            FDTYPE = FDTYP__CAT6(FIC)
            FCSIZE = FCSIZ__CAT6(FIC)
            PTR = FPTR__CAT6(FIC)
            PTRN = FPTRN__CAT6(FIC)
         ELSE
            STATUS = CAT__INVID
         END IF

*
*       Proceed if all is ok.

         IF (STATUS .EQ. CAT__OK) THEN
            ROWS = EROW__CAT1(CIELM)
C           print4200, rows
C4200       format(1x, 'rows: ', i5)

*
*          Check whether the value is to be got or put.

            IF (IOFLG) THEN

*
*             A value is to be got.  First get the null value flag.
*             If the value is not null then get the actual datum,
*             otherwise substitute the appropriate null value.

               CALL CAT6_GTAEL (ROWS, ROWNO, %VAL(CNF_PVAL(PTRN)),
     :                          NULFLG, STATUS)

               IF (.NOT. NULFLG) THEN

                  IF (FDTYPE .EQ. CAT__TYPEB) THEN
                     CALL CAT6_GTAEB (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                BVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPEW) THEN
                     CALL CAT6_GTAEW (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                WVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPEI) THEN
                     CALL CAT6_GTAEI (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                IVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPER) THEN
                     CALL CAT6_GTAER (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                RVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPED) THEN
                     CALL CAT6_GTAED (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                DVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPEL) THEN
                     CALL CAT6_GTAEL (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                LVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPEC) THEN
                     CALL CAT6_GTCAE(ROWS, FCSIZE, ROWNO,
     :                               %VAL(CNF_PVAL(PTR)),
     :                 CVALI, STATUS)
                  ELSE
                     STATUS = CAT__INVDT
                  END IF

                  CALL CAT1_TCNVT (FDTYPE, UBVALI, BVALI, UWVALI,
     :              WVALI, IVALI, RVALI, DVALI, LVALI, CVALI,
     :              CAT__TYPEL, UBVALO, BVALO, UWVALO, WVALO,
     :              IVALO, RVALO, DVALO, LVALO, CVALO, CONVOK,
     :              STATUS)

                  IF (CONVOK) THEN
                     VALUE = LVALO
                  ELSE
                     NULFLG = .TRUE.
                     VALUE = CAT1__DNULL
                  END IF

               ELSE
                  VALUE = CAT1__DNULL
               END IF

            ELSE

*
*             A value is to be put.

               IF (.NOT. NULFLG) THEN
                  LVALI = VALUE

                  CALL CAT1_TCNVT (CAT__TYPEL, UBVALI, BVALI, UWVALI,
     :              WVALI, IVALI, RVALI, DVALI, LVALI, CVALI,
     :              FDTYPE, UBVALO, BVALO, UWVALO, WVALO,
     :              IVALO, RVALO, DVALO, LVALO, CVALO, CONVOK,
     :              STATUS)

                  IF (CONVOK) THEN
                     IF (FDTYPE .EQ. CAT__TYPEB) THEN
                        CALL CAT6_STAEB (ROWS, ROWNO, BVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPEW) THEN
                        CALL CAT6_STAEW (ROWS, ROWNO, WVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPEI) THEN
                        CALL CAT6_STAEI (ROWS, ROWNO, IVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPER) THEN
                        CALL CAT6_STAER (ROWS, ROWNO, RVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPED) THEN
                        CALL CAT6_STAED (ROWS, ROWNO, DVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPEL) THEN
                        CALL CAT6_STAEL (ROWS, ROWNO, LVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPEC) THEN
                        CALL CAT6_STCAE(ROWS, FCSIZE, ROWNO, CVALO,
     :                    %VAL(CNF_PVAL(PTR)), STATUS)
                     ELSE
                        STATUS = CAT__INVDT
                     END IF

                     CALL CAT6_STAEL (ROWS, ROWNO, .FALSE.,
     :                                %VAL(CNF_PVAL(PTRN)),
     :                 STATUS)
                  ELSE
                     CALL CAT6_STAEL (ROWS, ROWNO, .TRUE.,
     :                                %VAL(CNF_PVAL(PTRN)),
     :                 STATUS)
                  END IF
               ELSE
                  CALL CAT6_STAEL (ROWS, ROWNO, .TRUE.,
     :                             %VAL(CNF_PVAL(PTRN)),
     :              STATUS)
               END IF

            END IF
         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            ERRPOS = 0
            ERRMSG = ' '

            CALL CHR_PUTC ('Failure getting field from TST catalogue '/
     :        /'(column: ', ERRMSG, ERRPOS)

            LRSTAT = CAT__OK
            CALL CAT_TIQAC (FI, 'NAME', FNAME, LRSTAT)

            IF (LRSTAT .EQ. CAT__OK  .AND.  FNAME .NE. ' ') THEN
               LFNAME = CHR_LEN(FNAME)
               CALL CHR_PUTC (FNAME(1 : LFNAME), ERRMSG, ERRPOS)
            ELSE
               CALL CHR_PUTC ('<unknown>', ERRMSG, ERRPOS)
            END IF

            CALL CHR_PUTC (', element: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ELEM, ERRMSG, ERRPOS)
            CALL CHR_PUTC (', row: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ROWNO, ERRMSG, ERRPOS)
            CALL CHR_PUTC (').', ERRMSG, ERRPOS)

            CALL CAT1_ERREP ('CAT6_FIOL_ERR', ERRMSG(1 : ERRPOS),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT6_FIOR (IOFLG, CIELM, FI, ELEM, ROWNO, VALUE,
     :  NULFLG, STATUS)
*+
*  Name:
*     CAT6_FIOR
*  Purpose:
*     Get or put a value for a single field.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT6_FIOR (IOFLG, CIELM, FI, ELEM, ROWNO; VALUE, NULFLG;
*       STATUS)
*  Description:
*     Get or put a value for a single field.
*  Arguments:
*     IOFLG  =  LOGICAL (Given)
*        Flag indicating whether the routine is to get or put a value,
*        coded as follows:
*        .TRUE.  - get a value,
*        .FALSE. - put a value.
*        Not used in this ADC read-only version.
*     CIELM  =  INTEGER (Given)
*        Array element in the catalogue common blocks corresponding to
*        the catalogue to be accessed.
*     FI  =  INTEGER (Given)
*        Identifier to the field to be put or got.
*     ELEM  =  INTEGER (Given)
*        Element of the array which is to be put or got.
*     ROWNO  =  INTEGER (Given)
*        Number of the row in which a field is to be accessed.
*     VALUE  =  REAL (Given and Returned)
*        Value to be put or obtained.
*     NULFLG  =  LOGICAL (Given and Returned)
*        Flag indicating whether or not the value is null.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Check whether the identifier corresponds to a scalar or a
*     vector.
*     If the identifier belongs to a scalar then
*       Copy the identifier
*     else (the identifier belongs to a vector)
*       Get the name of the column
*       Generate the name of the required vector element.
*       Get an identifier for this vector element.
*     end if
*     Attempt to obtain the pointers to the arrays holding the column
*     values and null flags.
*     If ok then
*       If getting a value then
*         Get the null value flag for the field.
*         If not null then
*           Get the actual value.
*         else
*           Adopt the appropriate null value.
*         end if
*       else (putting a value)
*         Set the value.
*         Set the null value flag.
*       end if
*     end if
*     Report any error.
*  Implementation Deficiencies:
*     Unsigned byte and unsigned word data types are not supported.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     15/6/99 (ACD): Original version (from CAT5_FIOT.GEN).
*     15/6/99 (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT1_NUL'          ! CAT null values.
*  Global Variables:
      INCLUDE 'CAT1_CATS_CMN'     ! Catalogues common block.
      INCLUDE 'CAT1_IDS_CMN'      ! Identifiers common block.
      INCLUDE 'CAT6_TST_CMN'      ! TST back-end common block.
      INCLUDE 'CNF_PAR'           ! For CNF_PVAL function
*  Arguments Given:
      LOGICAL
     :  IOFLG
      INTEGER
     :  CIELM,
     :  FI,
     :  ELEM,
     :  ROWNO
*  Arguments Given and Returned:
      REAL
     :  VALUE
      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS        ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      LOGICAL
     :  CONVOK    ! Flag; converted between data types ok?
      INTEGER
     :  FDIM,     ! Dimensionality of the column.
     :  FIC,      ! Identifier for the column or array element.
     :  LENAME,   ! Length of ENAME (excl. trail. blanks).
     :  FDTYPE,   ! Data type of the field.
     :  FCSIZE,   ! Size of the field if it is of type character.
     :  PTR,      ! Pointer to array of column values.
     :  PTRN,     ! Pointer to array of column null value flags.
     :  ROWS      ! Number of rows in the catalogue.
      CHARACTER
     :  ENAME*(CAT__SZCMP+4), ! Name of the required vector element.
     :  ERRMSG*75,            ! Text of error message.
     :  FNAME*(CAT__SZCMP)    ! Name of the current column.
      INTEGER
     :  ERRPOS,   ! Length of ERRMSG (excl. trail. blanks).
     :  LFNAME,   !   "    "  FNAME  ( "  .   "  .   "   ).
     :  LRSTAT    ! Local status, used when assembling error text.

*
*    'Input' variables for each data type.

      BYTE             UBVALI              ! Unsigned byte.
      BYTE             BVALI               ! Byte.
      INTEGER*2        UWVALI              ! Unsigned word.
      INTEGER*2        WVALI               ! word.
      INTEGER          IVALI               ! Integer.
      REAL             RVALI               ! Real.
      DOUBLE PRECISION DVALI               ! Double precision.
      LOGICAL          LVALI               ! Logical.
      CHARACTER        CVALI*(CAT__SZVAL)  ! Character.

*
*    'Output' variables for each data type.

      BYTE             UBVALO              ! Unsigned byte.
      BYTE             BVALO               ! Byte.
      INTEGER*2        UWVALO              ! Unsigned word.
      INTEGER*2        WVALO               ! word.
      INTEGER          IVALO               ! Integer.
      REAL             RVALO               ! Real.
      DOUBLE PRECISION DVALO               ! Double precision.
      LOGICAL          LVALO               ! Logical.
      CHARACTER        CVALO*(CAT__SZVAL)  ! Character.
*.

      IF (STATUS .EQ. CAT__OK) THEN

C       print4000, ioflg, cielm, fi, elem, rowno
C4000    format(1x, 'ioflg, cielm, fi, elem, rowno: ',
C    :     l5, i5, i5, i5, i5)

*
*       Check whether the identifier corresponds to a scalar or a
*       vector and then obtain the appropriate identifier.

         CALL CAT_TIQAI (FI, 'DIMS', FDIM, STATUS)

         IF (FDIM .EQ. CAT__SCALR) THEN
            FIC = FI

         ELSE
            CALL CAT_TIQAC (FI, 'NAME', FNAME, LRSTAT)

            ENAME = ' '
            LENAME = 0

            LFNAME = CHR_LEN(FNAME)
            CALL CHR_PUTC (FNAME(1 : LFNAME), ENAME, LENAME)
            CALL CHR_PUTC ('[', ENAME, LENAME)
            CALL CHR_PUTI (ELEM, ENAME, LENAME)
            CALL CHR_PUTC (']', ENAME, LENAME)

            CALL CAT_TIDNT(CIDS__CAT1(CIELM), ENAME(1 : LENAME),
     :        FIC, STATUS)
         END IF

C        print4100, fic
C4100    format(1x, 'fic: ', i5 )

*
*       Attempt to obtain the pointers to the arrays holding the column
*       values and null flags.

         IF (FIC .GT. 0  .AND.  FIC .LE. NIDS__CAT1) THEN
            FDTYPE = FDTYP__CAT6(FIC)
            FCSIZE = FCSIZ__CAT6(FIC)
            PTR = FPTR__CAT6(FIC)
            PTRN = FPTRN__CAT6(FIC)
         ELSE
            STATUS = CAT__INVID
         END IF

*
*       Proceed if all is ok.

         IF (STATUS .EQ. CAT__OK) THEN
            ROWS = EROW__CAT1(CIELM)
C           print4200, rows
C4200       format(1x, 'rows: ', i5)

*
*          Check whether the value is to be got or put.

            IF (IOFLG) THEN

*
*             A value is to be got.  First get the null value flag.
*             If the value is not null then get the actual datum,
*             otherwise substitute the appropriate null value.

               CALL CAT6_GTAEL (ROWS, ROWNO, %VAL(CNF_PVAL(PTRN)),
     :                          NULFLG, STATUS)

               IF (.NOT. NULFLG) THEN

                  IF (FDTYPE .EQ. CAT__TYPEB) THEN
                     CALL CAT6_GTAEB (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                BVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPEW) THEN
                     CALL CAT6_GTAEW (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                WVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPEI) THEN
                     CALL CAT6_GTAEI (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                IVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPER) THEN
                     CALL CAT6_GTAER (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                RVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPED) THEN
                     CALL CAT6_GTAED (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                DVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPEL) THEN
                     CALL CAT6_GTAEL (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                LVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPEC) THEN
                     CALL CAT6_GTCAE(ROWS, FCSIZE, ROWNO,
     :                               %VAL(CNF_PVAL(PTR)),
     :                 CVALI, STATUS)
                  ELSE
                     STATUS = CAT__INVDT
                  END IF

                  CALL CAT1_TCNVT (FDTYPE, UBVALI, BVALI, UWVALI,
     :              WVALI, IVALI, RVALI, DVALI, LVALI, CVALI,
     :              CAT__TYPER, UBVALO, BVALO, UWVALO, WVALO,
     :              IVALO, RVALO, DVALO, LVALO, CVALO, CONVOK,
     :              STATUS)

                  IF (CONVOK) THEN
                     VALUE = RVALO
                  ELSE
                     NULFLG = .TRUE.
                     VALUE = CAT1__DNULR
                  END IF

               ELSE
                  VALUE = CAT1__DNULR
               END IF

            ELSE

*
*             A value is to be put.

               IF (.NOT. NULFLG) THEN
                  RVALI = VALUE

                  CALL CAT1_TCNVT (CAT__TYPER, UBVALI, BVALI, UWVALI,
     :              WVALI, IVALI, RVALI, DVALI, LVALI, CVALI,
     :              FDTYPE, UBVALO, BVALO, UWVALO, WVALO,
     :              IVALO, RVALO, DVALO, LVALO, CVALO, CONVOK,
     :              STATUS)

                  IF (CONVOK) THEN
                     IF (FDTYPE .EQ. CAT__TYPEB) THEN
                        CALL CAT6_STAEB (ROWS, ROWNO, BVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPEW) THEN
                        CALL CAT6_STAEW (ROWS, ROWNO, WVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPEI) THEN
                        CALL CAT6_STAEI (ROWS, ROWNO, IVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPER) THEN
                        CALL CAT6_STAER (ROWS, ROWNO, RVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPED) THEN
                        CALL CAT6_STAED (ROWS, ROWNO, DVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPEL) THEN
                        CALL CAT6_STAEL (ROWS, ROWNO, LVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPEC) THEN
                        CALL CAT6_STCAE(ROWS, FCSIZE, ROWNO, CVALO,
     :                    %VAL(CNF_PVAL(PTR)), STATUS)
                     ELSE
                        STATUS = CAT__INVDT
                     END IF

                     CALL CAT6_STAEL (ROWS, ROWNO, .FALSE.,
     :                                %VAL(CNF_PVAL(PTRN)),
     :                 STATUS)
                  ELSE
                     CALL CAT6_STAEL (ROWS, ROWNO, .TRUE.,
     :                                %VAL(CNF_PVAL(PTRN)),
     :                 STATUS)
                  END IF
               ELSE
                  CALL CAT6_STAEL (ROWS, ROWNO, .TRUE.,
     :                             %VAL(CNF_PVAL(PTRN)),
     :              STATUS)
               END IF

            END IF
         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            ERRPOS = 0
            ERRMSG = ' '

            CALL CHR_PUTC ('Failure getting field from TST catalogue '/
     :        /'(column: ', ERRMSG, ERRPOS)

            LRSTAT = CAT__OK
            CALL CAT_TIQAC (FI, 'NAME', FNAME, LRSTAT)

            IF (LRSTAT .EQ. CAT__OK  .AND.  FNAME .NE. ' ') THEN
               LFNAME = CHR_LEN(FNAME)
               CALL CHR_PUTC (FNAME(1 : LFNAME), ERRMSG, ERRPOS)
            ELSE
               CALL CHR_PUTC ('<unknown>', ERRMSG, ERRPOS)
            END IF

            CALL CHR_PUTC (', element: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ELEM, ERRMSG, ERRPOS)
            CALL CHR_PUTC (', row: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ROWNO, ERRMSG, ERRPOS)
            CALL CHR_PUTC (').', ERRMSG, ERRPOS)

            CALL CAT1_ERREP ('CAT6_FIOR_ERR', ERRMSG(1 : ERRPOS),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT6_FIOW (IOFLG, CIELM, FI, ELEM, ROWNO, VALUE,
     :  NULFLG, STATUS)
*+
*  Name:
*     CAT6_FIOW
*  Purpose:
*     Get or put a value for a single field.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT6_FIOW (IOFLG, CIELM, FI, ELEM, ROWNO; VALUE, NULFLG;
*       STATUS)
*  Description:
*     Get or put a value for a single field.
*  Arguments:
*     IOFLG  =  LOGICAL (Given)
*        Flag indicating whether the routine is to get or put a value,
*        coded as follows:
*        .TRUE.  - get a value,
*        .FALSE. - put a value.
*        Not used in this ADC read-only version.
*     CIELM  =  INTEGER (Given)
*        Array element in the catalogue common blocks corresponding to
*        the catalogue to be accessed.
*     FI  =  INTEGER (Given)
*        Identifier to the field to be put or got.
*     ELEM  =  INTEGER (Given)
*        Element of the array which is to be put or got.
*     ROWNO  =  INTEGER (Given)
*        Number of the row in which a field is to be accessed.
*     VALUE  =  INTEGER*2 (Given and Returned)
*        Value to be put or obtained.
*     NULFLG  =  LOGICAL (Given and Returned)
*        Flag indicating whether or not the value is null.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Check whether the identifier corresponds to a scalar or a
*     vector.
*     If the identifier belongs to a scalar then
*       Copy the identifier
*     else (the identifier belongs to a vector)
*       Get the name of the column
*       Generate the name of the required vector element.
*       Get an identifier for this vector element.
*     end if
*     Attempt to obtain the pointers to the arrays holding the column
*     values and null flags.
*     If ok then
*       If getting a value then
*         Get the null value flag for the field.
*         If not null then
*           Get the actual value.
*         else
*           Adopt the appropriate null value.
*         end if
*       else (putting a value)
*         Set the value.
*         Set the null value flag.
*       end if
*     end if
*     Report any error.
*  Implementation Deficiencies:
*     Unsigned byte and unsigned word data types are not supported.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     15/6/99 (ACD): Original version (from CAT5_FIOT.GEN).
*     15/6/99 (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT1_NUL'          ! CAT null values.
*  Global Variables:
      INCLUDE 'CAT1_CATS_CMN'     ! Catalogues common block.
      INCLUDE 'CAT1_IDS_CMN'      ! Identifiers common block.
      INCLUDE 'CAT6_TST_CMN'      ! TST back-end common block.
      INCLUDE 'CNF_PAR'           ! For CNF_PVAL function
*  Arguments Given:
      LOGICAL
     :  IOFLG
      INTEGER
     :  CIELM,
     :  FI,
     :  ELEM,
     :  ROWNO
*  Arguments Given and Returned:
      INTEGER*2
     :  VALUE
      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS        ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      LOGICAL
     :  CONVOK    ! Flag; converted between data types ok?
      INTEGER
     :  FDIM,     ! Dimensionality of the column.
     :  FIC,      ! Identifier for the column or array element.
     :  LENAME,   ! Length of ENAME (excl. trail. blanks).
     :  FDTYPE,   ! Data type of the field.
     :  FCSIZE,   ! Size of the field if it is of type character.
     :  PTR,      ! Pointer to array of column values.
     :  PTRN,     ! Pointer to array of column null value flags.
     :  ROWS      ! Number of rows in the catalogue.
      CHARACTER
     :  ENAME*(CAT__SZCMP+4), ! Name of the required vector element.
     :  ERRMSG*75,            ! Text of error message.
     :  FNAME*(CAT__SZCMP)    ! Name of the current column.
      INTEGER
     :  ERRPOS,   ! Length of ERRMSG (excl. trail. blanks).
     :  LFNAME,   !   "    "  FNAME  ( "  .   "  .   "   ).
     :  LRSTAT    ! Local status, used when assembling error text.

*
*    'Input' variables for each data type.

      BYTE             UBVALI              ! Unsigned byte.
      BYTE             BVALI               ! Byte.
      INTEGER*2        UWVALI              ! Unsigned word.
      INTEGER*2        WVALI               ! word.
      INTEGER          IVALI               ! Integer.
      REAL             RVALI               ! Real.
      DOUBLE PRECISION DVALI               ! Double precision.
      LOGICAL          LVALI               ! Logical.
      CHARACTER        CVALI*(CAT__SZVAL)  ! Character.

*
*    'Output' variables for each data type.

      BYTE             UBVALO              ! Unsigned byte.
      BYTE             BVALO               ! Byte.
      INTEGER*2        UWVALO              ! Unsigned word.
      INTEGER*2        WVALO               ! word.
      INTEGER          IVALO               ! Integer.
      REAL             RVALO               ! Real.
      DOUBLE PRECISION DVALO               ! Double precision.
      LOGICAL          LVALO               ! Logical.
      CHARACTER        CVALO*(CAT__SZVAL)  ! Character.
*.

      IF (STATUS .EQ. CAT__OK) THEN

C       print4000, ioflg, cielm, fi, elem, rowno
C4000    format(1x, 'ioflg, cielm, fi, elem, rowno: ',
C    :     l5, i5, i5, i5, i5)

*
*       Check whether the identifier corresponds to a scalar or a
*       vector and then obtain the appropriate identifier.

         CALL CAT_TIQAI (FI, 'DIMS', FDIM, STATUS)

         IF (FDIM .EQ. CAT__SCALR) THEN
            FIC = FI

         ELSE
            CALL CAT_TIQAC (FI, 'NAME', FNAME, LRSTAT)

            ENAME = ' '
            LENAME = 0

            LFNAME = CHR_LEN(FNAME)
            CALL CHR_PUTC (FNAME(1 : LFNAME), ENAME, LENAME)
            CALL CHR_PUTC ('[', ENAME, LENAME)
            CALL CHR_PUTI (ELEM, ENAME, LENAME)
            CALL CHR_PUTC (']', ENAME, LENAME)

            CALL CAT_TIDNT(CIDS__CAT1(CIELM), ENAME(1 : LENAME),
     :        FIC, STATUS)
         END IF

C        print4100, fic
C4100    format(1x, 'fic: ', i5 )

*
*       Attempt to obtain the pointers to the arrays holding the column
*       values and null flags.

         IF (FIC .GT. 0  .AND.  FIC .LE. NIDS__CAT1) THEN
            FDTYPE = FDTYP__CAT6(FIC)
            FCSIZE = FCSIZ__CAT6(FIC)
            PTR = FPTR__CAT6(FIC)
            PTRN = FPTRN__CAT6(FIC)
         ELSE
            STATUS = CAT__INVID
         END IF

*
*       Proceed if all is ok.

         IF (STATUS .EQ. CAT__OK) THEN
            ROWS = EROW__CAT1(CIELM)
C           print4200, rows
C4200       format(1x, 'rows: ', i5)

*
*          Check whether the value is to be got or put.

            IF (IOFLG) THEN

*
*             A value is to be got.  First get the null value flag.
*             If the value is not null then get the actual datum,
*             otherwise substitute the appropriate null value.

               CALL CAT6_GTAEL (ROWS, ROWNO, %VAL(CNF_PVAL(PTRN)),
     :                          NULFLG, STATUS)

               IF (.NOT. NULFLG) THEN

                  IF (FDTYPE .EQ. CAT__TYPEB) THEN
                     CALL CAT6_GTAEB (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                BVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPEW) THEN
                     CALL CAT6_GTAEW (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                WVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPEI) THEN
                     CALL CAT6_GTAEI (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                IVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPER) THEN
                     CALL CAT6_GTAER (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                RVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPED) THEN
                     CALL CAT6_GTAED (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                DVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPEL) THEN
                     CALL CAT6_GTAEL (ROWS, ROWNO, %VAL(CNF_PVAL(PTR)),
     :                                LVALI,
     :                 STATUS)
                  ELSE IF (FDTYPE .EQ. CAT__TYPEC) THEN
                     CALL CAT6_GTCAE(ROWS, FCSIZE, ROWNO,
     :                               %VAL(CNF_PVAL(PTR)),
     :                 CVALI, STATUS)
                  ELSE
                     STATUS = CAT__INVDT
                  END IF

                  CALL CAT1_TCNVT (FDTYPE, UBVALI, BVALI, UWVALI,
     :              WVALI, IVALI, RVALI, DVALI, LVALI, CVALI,
     :              CAT__TYPEW, UBVALO, BVALO, UWVALO, WVALO,
     :              IVALO, RVALO, DVALO, LVALO, CVALO, CONVOK,
     :              STATUS)

                  IF (CONVOK) THEN
                     VALUE = WVALO
                  ELSE
                     NULFLG = .TRUE.
                     VALUE = CAT1__DNULW
                  END IF

               ELSE
                  VALUE = CAT1__DNULW
               END IF

            ELSE

*
*             A value is to be put.

               IF (.NOT. NULFLG) THEN
                  WVALI = VALUE

                  CALL CAT1_TCNVT (CAT__TYPEW, UBVALI, BVALI, UWVALI,
     :              WVALI, IVALI, RVALI, DVALI, LVALI, CVALI,
     :              FDTYPE, UBVALO, BVALO, UWVALO, WVALO,
     :              IVALO, RVALO, DVALO, LVALO, CVALO, CONVOK,
     :              STATUS)

                  IF (CONVOK) THEN
                     IF (FDTYPE .EQ. CAT__TYPEB) THEN
                        CALL CAT6_STAEB (ROWS, ROWNO, BVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPEW) THEN
                        CALL CAT6_STAEW (ROWS, ROWNO, WVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPEI) THEN
                        CALL CAT6_STAEI (ROWS, ROWNO, IVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPER) THEN
                        CALL CAT6_STAER (ROWS, ROWNO, RVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPED) THEN
                        CALL CAT6_STAED (ROWS, ROWNO, DVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPEL) THEN
                        CALL CAT6_STAEL (ROWS, ROWNO, LVALO,
     :                                   %VAL(CNF_PVAL(PTR)),
     :                    STATUS)
                     ELSE IF (FDTYPE .EQ. CAT__TYPEC) THEN
                        CALL CAT6_STCAE(ROWS, FCSIZE, ROWNO, CVALO,
     :                    %VAL(CNF_PVAL(PTR)), STATUS)
                     ELSE
                        STATUS = CAT__INVDT
                     END IF

                     CALL CAT6_STAEL (ROWS, ROWNO, .FALSE.,
     :                                %VAL(CNF_PVAL(PTRN)),
     :                 STATUS)
                  ELSE
                     CALL CAT6_STAEL (ROWS, ROWNO, .TRUE.,
     :                                %VAL(CNF_PVAL(PTRN)),
     :                 STATUS)
                  END IF
               ELSE
                  CALL CAT6_STAEL (ROWS, ROWNO, .TRUE.,
     :                             %VAL(CNF_PVAL(PTRN)),
     :              STATUS)
               END IF

            END IF
         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            ERRPOS = 0
            ERRMSG = ' '

            CALL CHR_PUTC ('Failure getting field from TST catalogue '/
     :        /'(column: ', ERRMSG, ERRPOS)

            LRSTAT = CAT__OK
            CALL CAT_TIQAC (FI, 'NAME', FNAME, LRSTAT)

            IF (LRSTAT .EQ. CAT__OK  .AND.  FNAME .NE. ' ') THEN
               LFNAME = CHR_LEN(FNAME)
               CALL CHR_PUTC (FNAME(1 : LFNAME), ERRMSG, ERRPOS)
            ELSE
               CALL CHR_PUTC ('<unknown>', ERRMSG, ERRPOS)
            END IF

            CALL CHR_PUTC (', element: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ELEM, ERRMSG, ERRPOS)
            CALL CHR_PUTC (', row: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ROWNO, ERRMSG, ERRPOS)
            CALL CHR_PUTC (').', ERRMSG, ERRPOS)

            CALL CAT1_ERREP ('CAT6_FIOW_ERR', ERRMSG(1 : ERRPOS),
     :        STATUS)
         END IF

      END IF

      END
