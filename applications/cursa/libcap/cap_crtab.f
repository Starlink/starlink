      SUBROUTINE CAP_CRTAB (CIIN, CIOUT, NGRAT, FIGRAT, TYGRAT,
     :  NUMCOL, FIIN, FIOUT, STATUS)
*+
*  Name:
*     CAP_CRTAB
*  Purpose:
*     Compute and write the table for a graphics attribute list.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_CRTAB (CIIN, CIOUT, NGRAT, FIGRAT, TYGRAT,
*       NUMCOL, FIIN, FIOUT, STATUS)
*  Description:
*     Compute and write the table for a graphics attribute list.
*  Arguments:
*     CIIN  =  INTEGER (Given)
*        Identifier to the input target list.  Note that this identifier
*        may be a catalogue or index.
*     CIOUT  =  INTEGER (Given)
*        Identifier to the output graphics attribute list.
*     NGRAT  =  INTEGER (Given)
*        Number of graphics attributes columns.
*     FIGRAT(NGRAT)  =  INTEGER (Given)
*        Identifiers for the graphics attributes columns in the
*        output graphics attribute list.
*     TYGRAT(NGRAT)  =  INTEGER (Given)
*        The type of each graphics attribute column.
*     NUMCOL  =  INTEGER (Given)
*        Number of columns in the input target list.
*     FIIN(NUMCOL)  =  INTEGER (Given)
*        Identifiers for the columns in the input target list.
*     FIOUT(NUMCOL)  =  INTEGER (Given)
*        Identifiers for the corresponding columns in the output
*        graphics attribute list.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     For every graphic attribute column.
*       Set the appropriate identifier.
*     end for
*     For every column identifier in the input target list.
*       Get the data type of the column.
*       Get the dimensionality of the column.
*       If the column is a scalar then
*         Add the column to the list of columns to copy.
*       else (the column is a vector)
*         Determine the number of elements in the vector.
*         Determine the name of the vector.
*         For every element in the vector
*           Get an identifier for the element.
*           Add the element to the list of columns to copy.
*         end for
*       end if
*     end for
*     Determine the number of rows in the input target list.
*     For every row in the input target list.
*       Read the next row from the input target list.
*       For every graphics attribute column.
*         Set the default value.
*       end for
*       For every IF block
*         While (not done and there are more clauses in the IF block)
*           Increment to the next clause.
*           If the current row satisfies the criterion then
*             Set any values associated with the clause.
*           end if
*         end do
*       end for
*       For every graphics attribute column
*         Put the value to the catalogue.
*       end for
*       For every column
*         Check the data type of the column, and for the appropriate
*         data type:
*           Get the value for the current field from the input
*           catalogue.
*           Put the value to the current field in the output catalogue.
*         end case
*       end for
*       Append the current row to the output graphics attribute list.
*     end for
*     If not ok then
*       Report error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     12/8/96 (ACD): Original version (from CAP_CPTAB).
*     13/8/96 (ACD): First stable version.
*     6/6/97  (ACD): Changed the subroutine prefix from CIO to CAP.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'     ! Standard SAE symbolic constants.
      INCLUDE 'CAT_PAR'     ! CAT symbolic constants.
      INCLUDE 'CAT_ERR'     ! Symbolic constants for CAT error codes.
      INCLUDE 'CIO_PAR'     ! CIO parametric constants.
*  Global Variables:
      INCLUDE 'CIO_CMN'     ! CIO common block.
*  Arguments Given:
      INTEGER
     :  CIIN,
     :  CIOUT,
     :  NGRAT,
     :  FIGRAT(NGRAT),
     :  TYGRAT(NGRAT),
     :  NUMCOL,
     :  FIIN(NUMCOL),
     :  FIOUT(NUMCOL)
*  Status:
      INTEGER STATUS        ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  FIINC,   ! Identifier to current input  column.
     :  FIOUTC,  !     "      "     "    output   "   .
     :  IDTYPE,  ! Type of the identifier to current input column.
     :  CURCOL,  ! Number of the current column.
     :  NUMID,   ! Total number of identifiers (inc. vector elements).
     :  CURID,   ! Current identifier (inc. vector elements).
     :  FIINI(CAT__MXCOL),  ! List of input  identifiers (inc. vectors).
     :  FIOUTI(CAT__MXCOL), !  "   "  output      "      ( " .    "   ).
     :  FDTYPI(CAT__MXCOL), ! Data types corresponding to identifiers.
     :  LFNAME   ! Length of FNAME (excl. trail. blanks).
      INTEGER
     :  DTYPE,  ! Data type of current column.
     :  DIMS,   ! Dimensionality of the current column.
     :  SIZE,   ! Size (number of elements) of the current column.
     :  ELEM,   ! Current vector element.
     :  ENMPOS, ! Current position in ENAME.
     :  FIINE,  ! Identifier to current input  scalar or vector element.
     :  FIOUTE, !     "      "     "    output   "    "    "       "   .
     :  ROWS,   ! Number of rows in the input catalogue.
     :  ROW     ! Current row.
      INTEGER
     :  CIINT,  ! Type of input catalogue: catalogue or index.
     :  CIINB   ! Identifier for base input catalogue.
      CHARACTER
     :  FNAME*(CAT__SZCNM),  ! Name of the current columnt.
     :  ENAME*(CAT__SZCNM)   ! Name of the current vector element.
      LOGICAL
     :  NULFLG  ! Flag; is the current field null?

*
*    The following values hold the value read (and written) for the
*    current field.  There are values for each of the data types
*    supported by SCAR/ADC.

      BYTE             VALUEB
      INTEGER*2        VALUEW
      INTEGER          VALUEI
      REAL             VALUER
      DOUBLE PRECISION VALUED
      LOGICAL          VALUEL
      CHARACTER        VALUEC*(CAT__SZVAL)

      INTEGER
     :  FISYMB,   ! Identifier to plotting symbol column.
     :  FICOLR,   !     "      "     "     colour   "   .
     :  FIUNIT,   !     "      "     "     units    "   .
     :  FILABL,   !     "      "     "     label    "   .
     :  FISIZ1,   !     "      "  first  plotting size column.
     :  FISIZ2,   !     "      "  second    "      "     "   .
     :  FISIZ3,   !     "      "  third     "      "     "   .
     :  FISIZ4    !     "      "  fourth    "      "     "   .
      INTEGER
     :  CURGAT,   ! Current graphics attribute.
     :  CURBLK,   !    "    IF block.
     :  CURCLS,   !    "    clause in IF block.
     :  NCLS,     ! Number of clauses in the current IF block.
     :  SYMBOL,   ! Current value for the plotting symbol.
     :  COLOUR,   !    "      "    "   "     "     colour.
     :  SUNITS    !    "      "    "   "     "     units.
      CHARACTER
     :  LABEL*(CAT__SZCMP) ! Current value for the plotting label.
      REAL
     :  SIZE1,    ! Current value for the first  plotting size.
     :  SIZE2,    !    "      "    "   "  second    "      "  .
     :  SIZE3,    !    "      "    "   "  third     "      "  .
     :  SIZE4     !    "      "    "   "  fourth    "      "  .
      LOGICAL
     :  SIZE1N,   ! Null value flag corresponding to SIZE1.
     :  SIZE2N,   !  "     "    "         "        " SIZE2.
     :  SIZE3N,   !  "     "    "         "        " SIZE3.
     :  SIZE4N,   !  "     "    "         "        " SIZE4.
     :  MEETCT,   ! Flag; does current row meet the criterion?
     :  MEETNL,   ! Null value flag corresponding to MEETCT.
     :  MORE      ! Flag; more IF clauses to process?
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Set the appropriate attribute for every graphics attribute
*       column.

         FISYMB = CIO__NULL
         FICOLR = CIO__NULL
         FIUNIT = CIO__NULL
         FILABL = CIO__NULL
         FISIZ1 = CIO__NULL
         FISIZ2 = CIO__NULL
         FISIZ3 = CIO__NULL
         FISIZ4 = CIO__NULL

         DO CURGAT = 1, NGRAT
            IF (TYGRAT(CURGAT) .EQ. CIO__TYSMB) THEN
               FISYMB = FIGRAT(CURGAT)
            END IF

            IF (TYGRAT(CURGAT) .EQ. CIO__TYCOL) THEN
               FICOLR = FIGRAT(CURGAT)
            END IF

            IF (TYGRAT(CURGAT) .EQ. CIO__TYUNT) THEN
               FIUNIT = FIGRAT(CURGAT)
            END IF

            IF (TYGRAT(CURGAT) .EQ. CIO__TYLBL) THEN
               FILABL = FIGRAT(CURGAT)
            END IF

            IF (TYGRAT(CURGAT) .EQ. CIO__TYSZ1) THEN
               FISIZ1 = FIGRAT(CURGAT)
            END IF

            IF (TYGRAT(CURGAT) .EQ. CIO__TYSZ2) THEN
               FISIZ2 = FIGRAT(CURGAT)
            END IF

            IF (TYGRAT(CURGAT) .EQ. CIO__TYSZ3) THEN
               FISIZ3 = FIGRAT(CURGAT)
            END IF

            IF (TYGRAT(CURGAT) .EQ. CIO__TYSZ4) THEN
               FISIZ4 = FIGRAT(CURGAT)
            END IF
         END DO

         NUMID = 0

*
*       Determine the data type and dimensionality of every column in
*       the input target list.  If the column is a vector then get
*       identifiers for all the individual elements.

         DO CURCOL = 1, NUMCOL
            FIINC = FIIN(CURCOL)
            FIOUTC = FIOUT(CURCOL)

*
*          Get the data type and dimensionality of the column.  Note
*          that expressions are scalars with a data type of DOUBLE
*          PRECISION.

            CALL CAT_TIDTP (FIINC, IDTYPE, STATUS)

            IF (IDTYPE .NE. CAT__EITYP) THEN
               CALL CAT_TIQAI (FIINC, 'DTYPE', DTYPE, STATUS)
               CALL CAT_TIQAI (FIINC, 'DIMS', DIMS, STATUS)
            ELSE
               DTYPE = CAT__TYPED
               DIMS = CAT__SCALR
            END IF

*
*          Check whether the column is a scalar or a vector.

            IF (DIMS .EQ. CAT__SCALR) THEN

*
*             Add the identifier to the list of columns to copy.

               IF (STATUS .EQ. SAI__OK) THEN
                  IF (NUMID .LT. CAT__MXCOL) THEN
                     NUMID = NUMID + 1

                     FIINI(NUMID) = FIINC
                     FIOUTI(NUMID) = FIOUTC
                     FDTYPI(NUMID) = DTYPE
                  END IF
               END IF
            ELSE

*
*             The column is a vector; determine the number of elements
*             and its name, then get an identifier for each element.

               CALL CAT_TIQAI (FIINC, 'SIZE', SIZE, STATUS)
               CALL CAT_TIQAC (FIINC, 'NAME', FNAME, STATUS)

               DO ELEM = 1, SIZE
                  ENAME = ' '
                  ENMPOS = 0

                  IF (FNAME .NE. ' ') THEN
                     LFNAME = CHR_LEN(FNAME)
                  ELSE
                     LFNAME = 1
                  END IF

                  CALL CHR_PUTC (FNAME(1 : LFNAME), ENAME, ENMPOS)
                  CALL CHR_PUTC ('[', ENAME, ENMPOS)
                  CALL CHR_PUTI (ELEM, ENAME, ENMPOS)
                  CALL CHR_PUTC (']', ENAME, ENMPOS)

                  CALL CAT_TIDTP (CIIN, CIINT, STATUS)

                  IF (CIINT .EQ. CAT__IITYP) THEN
                     CALL CAT_TIDPR (CIIN, CIINB, STATUS)
                  ELSE
                     CIINB = CIIN
                  END IF

                  CALL CAT_TIDNT (CIINB, ENAME, FIINE, STATUS)
                  CALL CAT_TIDNT (CIOUT, ENAME, FIOUTE, STATUS)

                  IF (STATUS .EQ. SAI__OK) THEN
                     IF (NUMID .LT. CAT__MXCOL) THEN
                        NUMID = NUMID + 1

                        FIINI(NUMID) = FIINE
                        FIOUTI(NUMID) = FIOUTE
                        FDTYPI(NUMID) = DTYPE
                     END IF
                  END IF
               END DO

            END IF
         END DO

*
*       Determine the number of rows in the input target list.

         CALL CAT_TROWS (CIIN, ROWS, STATUS)

*
*       For every row in the graphics attribute list write the
*       fields for the graphics attributes and copy all the fields
*       from the input target list.

         DO ROW = 1, ROWS

*
*          Read the next row from the input catalogue.

            CALL CAT_RGET (CIIN, ROW, STATUS)

*
*          Set the default value for every graphics attribute field.

            SYMBOL = DSYMB__CIO
            COLOUR = DCOLR__CIO
            SUNITS = DUNIT__CIO
            LABEL = DLABL__CIO

            IF (FISIZ1 .NE. CIO__NULL) THEN
               CALL CAT_EGT0R (DEIS1__CIO, SIZE1, SIZE1N, STATUS)
            END IF

            IF (FISIZ2.NE. CIO__NULL) THEN
               CALL CAT_EGT0R (DEIS2__CIO, SIZE2, SIZE2N, STATUS)
            END IF

            IF (FISIZ3 .NE. CIO__NULL) THEN
               CALL CAT_EGT0R (DEIS3__CIO, SIZE3, SIZE3N, STATUS)
            END IF

            IF (FISIZ4 .NE. CIO__NULL) THEN
               CALL CAT_EGT0R (DEIS4__CIO, SIZE4, SIZE4N, STATUS)
            END IF

*
*          Check every clause in every IF block and reset any values
*          that meet the various criteria.

            DO CURBLK = 1, NIFB__CIO
               NCLS = NCLS__CIO(CURBLK)

               MORE = .TRUE.
               CURCLS = 0

               DO WHILE (MORE)
                  CURCLS = CURCLS + 1

                  IF (CRIT__CIO(CURBLK, CURCLS) .NE. '<default>') THEN
                     CALL CAT_EGT0L (EICT__CIO(CURBLK, CURCLS),
     :                 MEETCT, MEETNL, STATUS)
                  ELSE
                     MEETCT = .TRUE.
                  END IF

                  IF (MEETCT) THEN
                     IF (SYMB__CIO(CURBLK, CURCLS) .NE. CIO__NULL) THEN
                        SYMBOL = SYMB__CIO(CURBLK, CURCLS)
                     END IF

                     IF (COLR__CIO(CURBLK, CURCLS) .NE. CIO__NULL) THEN
                        COLOUR = COLR__CIO(CURBLK, CURCLS)
                     END IF

                     IF (UNIT__CIO(CURBLK, CURCLS) .NE. CIO__NULL) THEN
                        SUNITS = UNIT__CIO(CURBLK, CURCLS)
                     END IF

                     IF (LABL__CIO(CURBLK, CURCLS) .NE. ' ') THEN
                        LABEL = LABL__CIO(CURBLK, CURCLS)
                     END IF

                     IF (SIZ1__CIO(CURBLK, CURCLS) .NE. ' ') THEN
                        CALL CAT_EGT0R (EIS1__CIO(CURBLK, CURCLS),
     :                    SIZE1, SIZE1N, STATUS)
                     END IF

                     IF (SIZ2__CIO(CURBLK, CURCLS) .NE. ' ') THEN
                        CALL CAT_EGT0R (EIS2__CIO(CURBLK, CURCLS),
     :                    SIZE2, SIZE2N, STATUS)
                     END IF

                     IF (SIZ3__CIO(CURBLK, CURCLS) .NE. ' ') THEN
                        CALL CAT_EGT0R (EIS3__CIO(CURBLK, CURCLS),
     :                    SIZE3, SIZE3N, STATUS)
                     END IF

                     IF (SIZ4__CIO(CURBLK, CURCLS) .NE. ' ') THEN
                        CALL CAT_EGT0R (EIS3__CIO(CURBLK, CURCLS),
     :                    SIZE4, SIZE4N, STATUS)
                     END IF

                     MORE = .FALSE.
                  END IF

                  IF (CURCLS .GE. NCLS) THEN
                     MORE = .FALSE.
                  END IF
               END DO
            END DO

*
*          For every graphics attribute column put a value to the
*          catalogue.

            IF (FISYMB .NE. CIO__NULL) THEN
               CALL CAT_PUT0I (FISYMB, SYMBOL, .FALSE., STATUS)
            END IF

            IF (FICOLR .NE. CIO__NULL) THEN
               CALL CAT_PUT0I (FICOLR, COLOUR, .FALSE., STATUS)
            END IF

            IF (FIUNIT .NE. CIO__NULL) THEN
               CALL CAT_PUT0I (FIUNIT, SUNITS, .FALSE., STATUS)
            END IF

            IF (FILABL .NE. CIO__NULL) THEN
               CALL CAT_PUT0C (FILABL, LABEL, .FALSE., STATUS)
            END IF

            IF (FISIZ1 .NE. CIO__NULL) THEN
               CALL CAT_PUT0R (FISIZ1, SIZE1, SIZE1N, STATUS)
            END IF

            IF (FISIZ2 .NE. CIO__NULL) THEN
               CALL CAT_PUT0R (FISIZ2, SIZE2, SIZE2N, STATUS)
            END IF

            IF (FISIZ3 .NE. CIO__NULL) THEN
               CALL CAT_PUT0R (FISIZ3, SIZE3, SIZE3N, STATUS)
            END IF

            IF (FISIZ4 .NE. CIO__NULL) THEN
               CALL CAT_PUT0R (FISIZ4, SIZE4, SIZE4N, STATUS)
            END IF

            DO CURID = 1, NUMID

*
*             Check the data type of the column, and for the appropriate
*             data type:
*              Get the value for the current field from the input
*              catalogue.
*              Put the value to the current field in the output
*              catalogue.

               IF (FDTYPI(CURID) .EQ. CAT__TYPEB) THEN
                  CALL CAT_EGT0B (FIINI(CURID), VALUEB, NULFLG,
     :              STATUS)
                  CALL CAT_PUT0B (FIOUTI(CURID), VALUEB, NULFLG,
     :              STATUS)

               ELSE IF (FDTYPI(CURID) .EQ. CAT__TYPEW) THEN
                  CALL CAT_EGT0W (FIINI(CURID), VALUEW, NULFLG,
     :              STATUS)
                  CALL CAT_PUT0W (FIOUTI(CURID), VALUEW, NULFLG,
     :              STATUS)

               ELSE IF (FDTYPI(CURID) .EQ. CAT__TYPEI) THEN
                  CALL CAT_EGT0I (FIINI(CURID), VALUEI, NULFLG,
     :              STATUS)
                  CALL CAT_PUT0I (FIOUTI(CURID), VALUEI, NULFLG,
     :              STATUS)

               ELSE IF (FDTYPI(CURID) .EQ. CAT__TYPER) THEN
                  CALL CAT_EGT0R (FIINI(CURID), VALUER, NULFLG,
     :              STATUS)
                  CALL CAT_PUT0R (FIOUTI(CURID), VALUER, NULFLG,
     :              STATUS)

               ELSE IF (FDTYPI(CURID) .EQ. CAT__TYPED) THEN
                  CALL CAT_EGT0D (FIINI(CURID), VALUED, NULFLG,
     :              STATUS)
                  CALL CAT_PUT0D (FIOUTI(CURID), VALUED, NULFLG,
     :              STATUS)

               ELSE IF (FDTYPI(CURID) .EQ. CAT__TYPEL) THEN
                  CALL CAT_EGT0L (FIINI(CURID), VALUEL, NULFLG,
     :              STATUS)
                  CALL CAT_PUT0L (FIOUTI(CURID), VALUEL, NULFLG,
     :              STATUS)

               ELSE IF (FDTYPI(CURID) .EQ. CAT__TYPEC) THEN
                  CALL CAT_EGT0C (FIINI(CURID), VALUEC, NULFLG,
     :              STATUS)
                  CALL CAT_PUT0C (FIOUTI(CURID), VALUEC, NULFLG,
     :              STATUS)

               ELSE
                  STATUS = CAT__INVDT

               END IF
            END DO

*
*          Append the current row to the output catalogue.

            CALL CAT_RAPND (CIOUT, STATUS)
         END DO

*
*       If the status is not ok then report an error.

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP ('CAP_CPTAB_ERR', 'Error copying catalogue '/
     :        /'table.', STATUS)
         END IF

      END IF

      END
