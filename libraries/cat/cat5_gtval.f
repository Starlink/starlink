      SUBROUTINE CAT5_GTVAL (FIELD, FDTYPE, FCSIZE, FSCLFL, FSCALE,
     :  FZERO, FANGLE, NANGLE, PTR, PTRN, ROWS, ROW, STATUS)
*+
*  Name:
*     CAT5_GTVAL
*  Purpose:
*     Copy value from record field to table array.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT5_GTVAL (FIELD, FDTYPE, FSCLFL, FSCALE, FZERO, FANGLE,
*       NANGLE, PTR, PTRN, ROWS, ROW; STATUS)
*  Description:
*     Copy value from record field to table array.
*  Arguments:
*     FIELD  =  CHARACTER*(*) (Given)
*        Field containing the value read from the current record.
*     FDTYPE  =  INTEGER (Given)
*        Data type of the column.
*     FCSIZE  =  INTEGER (Given
*        Size of the column if it is of type character, otherwise
*        unused.
*     FSCLFL  =  LOGICAL (Given)
*        Scaled flag for the column.
*     FSCALE  =  DOUBLE PRECISION (Given)
*        Scale factor of the column is scaled.
*     FZERO  =  DOUBLE PRECISION (Given)
*        Zero point if the column is scaled.
*     FANGLE  =  INTEGER (Given)
*        Code indicating whether the value is an angle, and if so what
*        its units are.
*     NANGLE  =  INTEGER (Given)
*        If the column is an angle then its sequence number amongst the
*        columns of angles, otherwise zero.
*     PTR  =  INTEGER (Given)
*        Pointer to array to hold the values for the column.
*     PTRN  =  INTEGER (Given)
*        Pointer to array holding the null value flags for the column.
*     ROWS  =  INTEGER (Given)
*        Number of rows in the table array.
*     ROW  =  INTEGER (Given)
*        Current row in the table array.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the column is not scaled then
*       If the column is not an angle then
*         Convert the value to the appropriate data type.
*       else
*         Convert the angle to radians.
*         Convert the value to the appropriate data type.
*       end if
*     else (the column is scaled)
*       Get the value as a double precision value.
*       Apply the scaling.
*       Convert the value to the appropriate data type.
*     end if
*     If the value converted ok
*       For the appropiate data type
*         Copy the value to the column
*       end for
*     else
*       Set the null value.
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
*     12/7/96 (ACD): Original version.
*     30/7/96 (ACD): First stable version.
*     4/8/98  (ACD): Added facilities for handling complex angles, in
*       addition to simple ones.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
*  Global Variables:
      INCLUDE 'CAT5_ANG_CMN'      ! STL angles common block.
      INCLUDE 'CNF_PAR'           ! For CNF_PVAL function
*  Arguments Given:
      CHARACTER
     :  FIELD*(*)
      INTEGER
     :  FDTYPE,
     :  FCSIZE,
     :  FANGLE,
     :  NANGLE,
     :  PTR,
     :  PTRN,
     :  ROWS,
     :  ROW
      LOGICAL
     :  FSCLFL
      DOUBLE PRECISION
     :  FSCALE,
     :  FZERO
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Variables:

*
*    Values for the various data types.

      BYTE             UBVAL              ! Unsigned byte.
      BYTE             BVAL               ! Byte.
      INTEGER*2        UWVAL              ! Unsigned word.
      INTEGER*2        WVAL               ! word.
      INTEGER          IVAL               ! Integer.
      INTEGER*8        KVAL               ! Integer*8.
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
      INTEGER*8        KDUM               ! Integer*8.
      REAL             RDUM               ! Real.
      DOUBLE PRECISION DDUM               ! Double precision.
      LOGICAL          LDUM               ! Logical.
      CHARACTER        CDUM*(CAT__SZVAL)  ! Character.

      LOGICAL
     :  CONVOK  ! Flag; conversion ok between data types?
      DOUBLE PRECISION
     :  SCALED, ! Value after applying scaling.
     :  ANGLE   ! Value of an angle in radians.
      INTEGER
     :  LCVAL   ! Length of CVAL (excl. trail. blanks).
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Check whether the column is scaled.

         IF (.NOT. FSCLFL) THEN

*
*          Check whether the column is an angle.

            IF (FANGLE .EQ. CAT1__NANGL) THEN

*
*             The column is neither scaled nor an angle; convert the
*             value to the appropriate data type.

               CALL CAT1_TCNVT (CAT__TYPEC, UBDUM, BDUM, UWDUM,
     :           WDUM, IDUM, KDUM, RDUM, DDUM, LDUM, FIELD,
     :           FDTYPE, UBVAL, BVAL, UWVAL, WVAL,
     :           IVAL, KVAL, RVAL, DVAL, LVAL, CVAL, CONVOK,
     :           STATUS)
            ELSE

*
*             The column is an angle.  Check whether the angle is
*             simple or complex and then attempt to convert the
*             value to radians in the appropriate fashion.  Then
*             convert it to the required data type.

               IF (.NOT. ANCMX__CAT5(NANGLE)) THEN
                  CALL CAT1_ANGDC (FIELD, FANGLE, ANGLE, CONVOK, STATUS)
               ELSE
                  CALL CAT5_ANCDC (FIELD, NANGLE, ANGLE, CONVOK, STATUS)
               END IF

               IF (CONVOK) THEN
                  CALL CAT1_TCNVT (CAT__TYPED, UBDUM, BDUM, UWDUM,
     :              WDUM, IDUM, KDUM, RDUM, ANGLE, LDUM, CDUM,
     :              FDTYPE, UBVAL, BVAL, UWVAL, WVAL,
     :              IVAL, KVAL, RVAL, DVAL, LVAL, CVAL, CONVOK,
     :              STATUS)
               END IF
            END IF

         ELSE
*
*          The column is scaled; convert the value to a double
*          precision number, apply the scaling then convert it to
*          the required data type.

            CALL CAT1_TCNVT (CAT__TYPEC, UBDUM, BDUM, UWDUM,
     :        WDUM, IDUM, KDUM, RDUM, DDUM, LDUM, FIELD,
     :        CAT__TYPED, UBVAL, BVAL, UWVAL, WVAL,
     :        IVAL, KVAL, RVAL, DVAL, LVAL, CVAL, CONVOK,
     :        STATUS)

            IF (CONVOK) THEN
               SCALED = (DVAL * FSCALE) + FZERO

               CALL CAT1_TCNVT (CAT__TYPED, UBDUM, BDUM, UWDUM,
     :           WDUM, IDUM, KDUM, RDUM, SCALED, LDUM, CDUM,
     :           FDTYPE, UBVAL, BVAL, UWVAL, WVAL,
     :           IVAL, KVAL, RVAL, DVAL, LVAL, CVAL, CONVOK,
     :           STATUS)
            END IF
         END IF

*
*       If the value converted ok then write it to the appropriate
*       array element.

         IF (CONVOK) THEN
            IF (FDTYPE .EQ. CAT__TYPEB) THEN
               CALL CAT5_STAEB (ROWS, ROW, BVAL, %VAL(CNF_PVAL(PTR)),
     :                          STATUS)
               CALL CAT5_STAEL (ROWS, ROW, .FALSE.,
     :                          %VAL(CNF_PVAL(PTRN)), STATUS)

            ELSE IF (FDTYPE .EQ. CAT__TYPEW) THEN
               CALL CAT5_STAEW (ROWS, ROW, WVAL, %VAL(CNF_PVAL(PTR)),
     :                          STATUS)
               CALL CAT5_STAEL (ROWS, ROW, .FALSE.,
     :                          %VAL(CNF_PVAL(PTRN)), STATUS)

            ELSE IF (FDTYPE .EQ. CAT__TYPEI) THEN
               CALL CAT5_STAEI (ROWS, ROW, IVAL, %VAL(CNF_PVAL(PTR)),
     :                          STATUS)
               CALL CAT5_STAEL (ROWS, ROW, .FALSE.,
     :                          %VAL(CNF_PVAL(PTRN)), STATUS)

            ELSE IF (FDTYPE .EQ. CAT__TYPEK) THEN
               CALL CAT5_STAEK (ROWS, ROW, IVAL, %VAL(CNF_PVAL(PTR)),
     :                          STATUS)
               CALL CAT5_STAEL (ROWS, ROW, .FALSE.,
     :                          %VAL(CNF_PVAL(PTRN)), STATUS)

            ELSE IF (FDTYPE .EQ. CAT__TYPER) THEN
               CALL CAT5_STAER (ROWS, ROW, RVAL, %VAL(CNF_PVAL(PTR)),
     :                          STATUS)
               CALL CAT5_STAEL (ROWS, ROW, .FALSE.,
     :                          %VAL(CNF_PVAL(PTRN)), STATUS)

            ELSE IF (FDTYPE .EQ. CAT__TYPED) THEN
               CALL CAT5_STAED (ROWS, ROW, DVAL, %VAL(CNF_PVAL(PTR)),
     :                          STATUS)
               CALL CAT5_STAEL (ROWS, ROW, .FALSE.,
     :                          %VAL(CNF_PVAL(PTRN)), STATUS)

            ELSE IF (FDTYPE .EQ. CAT__TYPEL) THEN
               CALL CAT5_STAEL (ROWS, ROW, LVAL, %VAL(CNF_PVAL(PTR)),
     :                          STATUS)
               CALL CAT5_STAEL (ROWS, ROW, .FALSE.,
     :                          %VAL(CNF_PVAL(PTRN)), STATUS)

            ELSE IF (FDTYPE .EQ. CAT__TYPEC) THEN
               IF (CVAL(1 : 1) .EQ. ''''  .OR.
     :             CVAL(1 : 1) .EQ. '"') THEN
                  CVAL(1 : 1) = ' '

                  CALL CHR_LDBLK (CVAL)

                  IF (CVAL .NE. ' ') THEN
                     LCVAL = CHR_LEN(CVAL)
                     CVAL(LCVAL : LCVAL) = ' '
                  END IF
               END IF

               CALL CAT5_STCAE (ROWS, FCSIZE, ROW, CVAL,
     :                          %VAL(CNF_PVAL(PTR)),
     :           STATUS)
               CALL CAT5_STAEL (ROWS, ROW, .FALSE.,
     :                          %VAL(CNF_PVAL(PTRN)), STATUS)

            ELSE
               CALL CAT5_STAEL (ROWS, ROW, .TRUE., %VAL(CNF_PVAL(PTRN)),
     :                          STATUS)

            END IF
         ELSE
            CALL CAT5_STAEL (ROWS, ROW, .TRUE., %VAL(CNF_PVAL(PTRN)),
     :                       STATUS)

         END IF

      END IF

      END
