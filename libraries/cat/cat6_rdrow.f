      SUBROUTINE CAT6_RDROW (RACOL, DECCOL, RAUNIT, ROWS, ROW, BUFFER,
     :  NUMCOL, FDTYPA, FCSIZA, FPTRA, FPTRNA, STATUS)
*+
*  Name:
*     CAT6_RDROW
*  Purpose:
*     Read the fields in a given row of a tab-separated table.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT6_RDROW (RACOL, DECCOL, RAUNIT, ROWS, ROW, BUFFER,
*       NUMCOL, FDTYPA, FCSIZA, FPTRA, FPTRNA; STATUS)
*  Description:
*     Read the fields in a given row of a tab-separated table.
*  Arguments:
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
*     ROWS  =  INTEGER (Given)
*        Number of rows in the table.
*     ROW  =  INTEGER (Given)
*        Number of the current row.
*     BUFFER  =  CHARACTER*(*) (Given)
*        Buffer holding the current row.
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
*     Split the row into fields.
*     For every field.
*       If the field is not null then
*         If the current column is the RA or DEC then
*           Attempt to decode the value as an RA or DEC
*           If ok then
*             Set the value.
*           else
*             Set the null value flag.
*           end if
*         else
*           Attempt to convert the character field to the required
*           Data type.
*           For the appropriate data type:
*             If ok then
*               Copy the value to the column array.
*               Set the null value flag.
*             else
*               Set the null value flag.
*             end if
*           end if
*         end if
*       else
*         Set the null value flag.
*       end if
*     end do
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
*     4/6/99  (ACD): Original version.
*     18/6/99 (ACD): First stable version.
*     12/7/00 (ACD): Removed the 'Interpretation mode'.
*     13/7/00 (ACD): Added support for all CURSA data types.
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
      INCLUDE 'CAT6_TST_CMN'     ! Tab-separated table common block.
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
*  Arguments Given:
      INTEGER
     :  RACOL,
     :  DECCOL,
     :  RAUNIT,
     :  ROWS,
     :  ROW,
     :  NUMCOL
      INTEGER
     :  FDTYPA(NUMCOL),
     :  FCSIZA(NUMCOL),
     :  FPTRA(NUMCOL),
     :  FPTRNA(NUMCOL)
      CHARACTER
     :  BUFFER*(*)
*  Status:
      INTEGER STATUS          ! Global status.
*  Local Variables:
      CHARACTER
     :  TAB*1,   ! Tab character.
     :  FIELD(CAT6__MXCOL)*(CAT__SZVAL)   ! Fields in the current row.
      LOGICAL
     :  NULFLG(CAT6__MXCOL),   ! Null value flags for the fields.
     :  CONVOK   ! Flag; angle decoded ok for current field?
      INTEGER
     :  NCOL,    ! No. of columns in the current row.
     :  CURCOL,  ! Current column.
     :  ANGUNT   ! Code for angular units for current field.

*
*    Values for the various data types.

      BYTE             UBVAL              ! Unsigned byte.
      BYTE             BVAL               ! Byte.
      INTEGER*2        UWVAL              ! Unsigned word.
      INTEGER*2        WVAL               ! word.
      INTEGER          IVAL               ! Integer.
      REAL             RVAL               ! Real.
      DOUBLE PRECISION DVAL               ! Double precision.
      LOGICAL          LVAL               ! Logical.
      CHARACTER        CVAL*(CAT__SZVAL)  ! Character.

*
*    Dummy values for the various data types.

      BYTE             UBDUM              ! Unsigned byte.
      BYTE             BDUM               ! Byte.
      INTEGER*2        UWDUM              ! Unsigned word.
      INTEGER*2        WDUM               ! word.
      INTEGER          IDUM               ! Integer.
      REAL             RDUM               ! Real.
      DOUBLE PRECISION DDUM               ! Double precision.
      LOGICAL          LDUM               ! Logical.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Set the tab character.

         TAB = CHAR(CAT6__TABI)

*
*       Split the row into its constituent fields.

         CALL CAT6_SPLIT (BUFFER, TAB, CAT6__MXCOL, NCOL, FIELD,
     :     NULFLG, STATUS)

*
*       Examine every field.

         NCOL = MIN(NCOL, NUMCOL)

         DO CURCOL = 1, NCOL

*
*          Check that the field is not null.

            IF (.NOT. NULFLG(CURCOL)) THEN

*
*             Attempt to obtain a value for the field.
*
*             First check whether current column corresponds to either
*             Right Ascension or Declination.

               IF (CURCOL .EQ. RACOL  .OR.  CURCOL .EQ. DECCOL) THEN

*
*                Attempt to decode the value as a Right Ascension or
*                Declination.

                  IF (CURCOL .EQ. RACOL) THEN
                     ANGUNT = RAUNIT
                  ELSE
                     ANGUNT = CAT1__DEG
                  END IF

                  CALL CAT1_ANGDC (FIELD(CURCOL), ANGUNT, DVAL, CONVOK,
     :              STATUS)
                  IF (CONVOK) THEN
                     CALL CAT6_STAED (ROWS, ROW, DVAL,
     :                 %VAL(CNF_PVAL(FPTRA(CURCOL))), STATUS)
                     CALL CAT6_STAEL (ROWS, ROW, .FALSE.,
     :                 %VAL(CNF_PVAL(FPTRNA(CURCOL))), STATUS)
                  ELSE
                     CALL CAT6_STAEL (ROWS, ROW, .TRUE.,
     :                 %VAL(CNF_PVAL(FPTRNA(CURCOL))), STATUS)
                  END IF

               ELSE

*
*                The column is not a Right Ascension or Declination;
*                interpret it as a simple value.
*
*                First attempt to convert the character field to the
*                approporiate data type.  Then copy the converted
*                value to the column array and set the null value flag.

                  CALL CAT1_TCNVT (CAT__TYPEC, UBDUM, BDUM, UWDUM,
     :              WDUM, IDUM, RDUM, DDUM, LDUM, FIELD(CURCOL),
     :              FDTYPA(CURCOL), UBVAL, BVAL, UWVAL, WVAL,
     :              IVAL, RVAL, DVAL, LVAL, CVAL, CONVOK,
     :              STATUS)

                  IF (CONVOK) THEN
                     IF (FDTYPA(CURCOL) .EQ. CAT__TYPEB) THEN
                        CALL CAT6_STAEB (ROWS, ROW, BVAL,
     :                    %VAL(CNF_PVAL(FPTRA(CURCOL))), STATUS)
                        CALL CAT6_STAEL (ROWS, ROW, .FALSE.,
     :                    %VAL(CNF_PVAL(FPTRNA(CURCOL))), STATUS)

                     ELSE IF (FDTYPA(CURCOL) .EQ. CAT__TYPEW) THEN
                        CALL CAT6_STAEW (ROWS, ROW, WVAL,
     :                    %VAL(CNF_PVAL(FPTRA(CURCOL))), STATUS)
                        CALL CAT6_STAEL (ROWS, ROW, .FALSE.,
     :                    %VAL(CNF_PVAL(FPTRNA(CURCOL))), STATUS)

                     ELSE IF (FDTYPA(CURCOL) .EQ. CAT__TYPEI) THEN
                        CALL CAT6_STAEI (ROWS, ROW, IVAL,
     :                    %VAL(CNF_PVAL(FPTRA(CURCOL))), STATUS)
                        CALL CAT6_STAEL (ROWS, ROW, .FALSE.,
     :                    %VAL(CNF_PVAL(FPTRNA(CURCOL))), STATUS)

                     ELSE IF (FDTYPA(CURCOL) .EQ. CAT__TYPER) THEN
                        CALL CAT6_STAER (ROWS, ROW, RVAL,
     :                    %VAL(CNF_PVAL(FPTRA(CURCOL))), STATUS)
                        CALL CAT6_STAEL (ROWS, ROW, .FALSE.,
     :                    %VAL(CNF_PVAL(FPTRNA(CURCOL))), STATUS)

                     ELSE IF (FDTYPA(CURCOL) .EQ. CAT__TYPED) THEN
                        CALL CAT6_STAED (ROWS, ROW, DVAL,
     :                    %VAL(CNF_PVAL(FPTRA(CURCOL))), STATUS)
                        CALL CAT6_STAEL (ROWS, ROW, .FALSE.,
     :                    %VAL(CNF_PVAL(FPTRNA(CURCOL))), STATUS)

                     ELSE IF (FDTYPA(CURCOL) .EQ. CAT__TYPEL) THEN
                        CALL CAT6_STAEL (ROWS, ROW, LVAL,
     :                    %VAL(CNF_PVAL(FPTRA(CURCOL))), STATUS)
                        CALL CAT6_STAEL (ROWS, ROW, .FALSE.,
     :                    %VAL(CNF_PVAL(FPTRNA(CURCOL))), STATUS)

                     ELSE IF (FDTYPA(CURCOL) .EQ. CAT__TYPEC) THEN
                        CALL CAT6_STCAE (ROWS, FCSIZA(CURCOL), ROW,
     :                    CVAL, %VAL(CNF_PVAL(FPTRA(CURCOL))), STATUS)
                        CALL CAT6_STAEL (ROWS, ROW, .FALSE.,
     :                    %VAL(CNF_PVAL(FPTRNA(CURCOL))), STATUS)

                     ELSE
                        CALL CAT6_STAEL (ROWS, ROW, .TRUE.,
     :                    %VAL(CNF_PVAL(FPTRNA(CURCOL))), STATUS)

                     END IF

                  ELSE
                     CALL CAT6_STAEL (ROWS, ROW, .TRUE.,
     :                 %VAL(CNF_PVAL(FPTRNA(CURCOL))), STATUS)

                  END IF

               END IF

            ELSE

*
*             The field was null in the table; set the null value flag.

               CALL CAT6_STAEL (ROWS, ROW, .TRUE.,
     :                          %VAL(CNF_PVAL(FPTRNA(CURCOL))),
     :           STATUS)
            END IF
         END DO

      END IF

      END
