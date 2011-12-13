      SUBROUTINE CAT_EGT0F (GI, VALUE, NULFLG, STATUS)
*+
*  Name:
*     CAT_EGT0F
*  Purpose:
*     Get the formatted value of a scalar exprn. field or parameter.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_EGT0F (GI; VALUE, NULFLG; STATUS)
*  Description:
*     Get the formatted value of a scalar expression, evaluated from the
*     current row buffer, field or parameter.  The value is formatted
*     into a CHARACTER string using the appropriate external format
*     specifier.
*
*     Remember that parameter and column identifiers correspond to
*     valid expressions.
*  Arguments:
*     GI  =  INTEGER (Given)
*        Identifier for either an expression, field or parameter.
*     VALUE  =  CHARACTER*(*) (Returned)
*        Value to which the expression evaluates for the current row
*        buffer.  If the expression evaluates to null the string
*        returned is '<null>' if VALUE contains six or more characters,
*        otherwise it is '?'.
*     NULFLG  =  LOGICAL (Returned)
*        A flag indicating whether or not the expression evaluates to
*        the null value or not:
*        .TRUE.  - The expression is null,
*        .FALSE. - The expression is not null; a genuine value is
*                  available.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the identifier corresponds to a vector element then
*       Get the corresponding base vector identifier
*     else
*       Copy the identifier.
*     end if
*     Determine the data type of the identifier.
*     Determine the external format of the identifier.
*     If angles are to be converted to sexagesimal form for output then
*       Get the units of the column.
*       Convert the units to upper case.
*       If the units are radians then
*         Generate a set of sexagesimal format descriptors for the
*         column.
*         Set the local angle conversion flag.
*       end if
*     end if
*     If ok then
*       If the data type is byte then
*         Get the value.
*         If the value is not null then
*           Attempt to write the value to the string using the external
*           format.
*           If not ok then
*             Attempt to write the value without the format specifier.
*           end if
*         end if
*       else if the data type is word then
*         .
*         . (repeat for the other data types).
*         .
*       else
*         Set the status to 'invalid data type'.
*       end if
*       If the value is null then
*         Write the appropriate 'null' message to the value string.
*       end if
*     end if
*     If any error occurred then
*       Report the error.
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
*     ACD: A C Davenhall (Leicester)
*  History:
*     10/2/94 (ACD): Original version.
*     9/3/95  (ACD): Added optional conversion of angles in radians to
*       sexagesimal form for output.
*     15/2/96 (ACD): Fixed a technical illegality in the use of the
*       concatention operator with a variable length string.
*     1/3/96  (ACD): Added support for minutes and seconds of arc and
*       time as units of angles.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Global Variables:
      INCLUDE 'CAT1_CATS_CMN'     ! Catalogues common block.
      INCLUDE 'CAT1_CTRL_CMN'     ! Control common block.
*  Arguments Given:
      INTEGER
     :  GI
*  Arguments Returned:
      CHARACTER
     :  VALUE*(*)
      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_SIZE
*  Local Constants:
      INTEGER IOOK   ! Internal Fortran I/O success status.
      PARAMETER (IOOK = 0)
*  Local Variables:
      INTEGER
     :  GIB,    ! Base identifier for vector column or parameter.
     :  IDTYPE, ! Type of the identifier.
     :  DTYPE,  ! Data type of column or parameter.
     :  LSTAT,  ! Local internal I/O status.
     :  VSIZE   ! Lenght of VALUE (including trail. blanks).
      CHARACTER
     :  EXFMT*(CAT__SZEXF), ! External format for column or parameter.
     :  UNITS*(CAT__SZUNI)  ! Units for column or parameter.
      INTEGER
     :  SEP,    ! Sexagesimal format descriptor: separator.
     :  UNIT,   !     "         "        "     : units (hours, degrees etc).
     :  NSGDIV, !     "         "        "     : no. sexagesimal subdiv'ns.
     :  DECPL   !     "         "        "     : no. of decimal places.
      LOGICAL
     :  PLUS,   !     "         "        "     : include + is positive.
     :  LEAD,   !     "         "        "     : include leading zeroes.
     :  ANGCVT  ! Flag: is column an angle requiring conversion?

*
*    Local copy of value of column or parameter.

      BYTE       VALUEB  ! byte,
      INTEGER*2  VALUEW  ! word,
      INTEGER    VALUEI  ! integer,
      REAL       VALUER  ! real,
      DOUBLE PRECISION  VALUED        ! double precision,
      LOGICAL    VALUEL  ! logical,
      CHARACTER  VALUEC*(CAT__SZVAL)  ! character.
*.

C     write(17, 2000) status
C2000 format(1x, 'EGT0F on entry: ', I10 )

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Check if the identifier corresponds to an array element and if
*       so get the base identifier; otherwise copy the identifier.

         CALL CAT_TIDTP (GI, IDTYPE, STATUS)

         IF (IDTYPE .EQ. CAT__FETYP  .OR.  IDTYPE .EQ. CAT__QETYP) THEN
            CALL CAT_TIQAI (GI, 'BASEID', GIB, STATUS)
         ELSE
            GIB = GI
         END IF

*
*       Attempt to determine the data type and external format for the
*       identifier and proceed if ok.

         CALL CAT_TIQAI (GIB, 'DTYPE', DTYPE, STATUS)
         CALL CAT_TIQAC (GIB, 'EXFMT', EXFMT, STATUS)

*
*       Check whether angles in radians are to be converted to
*       sexagesimal form prior to output.

         ANGCVT = .FALSE.

         IF (ANGCV__CAT1) THEN

*
*          Get the units and convert them to upper case.

            CALL CAT_TIQAC (GIB, 'UNITS', UNITS, STATUS)
            CALL CHR_UCASE (UNITS)

*
*          Check whether the current column (or parameter) is an angle.

            IF (UNITS(1 : 7) .EQ. 'RADIANS') THEN

*
*             Generate a set of sexagesimal format specifiers.

               CALL CAT1_ANPRS (UNITS, SEP, PLUS, LEAD, UNIT, NSGDIV,
     :           DECPL, STATUS)

*
*             Set the local flag to indicate than an angular conversion
*             will be carried out for this column (or parameter).

               ANGCVT = .TRUE.
            END IF
         END IF

         IF (STATUS .EQ. CAT__OK) THEN

*
*          Examine the data type and attempt to get a value of the
*          appropriate type.  Then try to write it to the return string
*          using the external format.  If this attempt fails attempt
*          to write the value to the return string *without* using
*          the external format.  If a null value was encountered then
*          substitute the appropriate 'null value' message in the
*          return string.
*
*          This process is repeated for each data type: first byte...

            IF (DTYPE .EQ. CAT__TYPEB) THEN
               CALL CAT_EGT0B (GI, VALUEB, NULFLG, STATUS)

               IF (STATUS .EQ. CAT__OK  .AND.  .NOT. NULFLG) THEN
                  WRITE(VALUE, FMT='(' // EXFMT // ')',
     :              IOSTAT=LSTAT) VALUEB

                  IF (LSTAT .NE. IOOK) THEN
                     CALL CAT_EGT0C (GI, VALUE, NULFLG, STATUS)
                  END IF

               END IF

*
*          Word...

            ELSE IF (DTYPE .EQ. CAT__TYPEW) THEN
               CALL CAT_EGT0W (GI, VALUEW, NULFLG, STATUS)

               IF (STATUS .EQ. CAT__OK  .AND.  .NOT. NULFLG) THEN
                  WRITE(VALUE, FMT='(' // EXFMT // ')',
     :              IOSTAT=LSTAT) VALUEW

                  IF (LSTAT .NE. IOOK) THEN
                     CALL CAT_EGT0C (GI, VALUE, NULFLG, STATUS)
                  END IF

               END IF

*
*          Integer...

            ELSE IF (DTYPE .EQ. CAT__TYPEI) THEN
               CALL CAT_EGT0I (GI, VALUEI, NULFLG, STATUS)

               IF (STATUS .EQ. CAT__OK  .AND.  .NOT. NULFLG) THEN
                  WRITE(VALUE, FMT='(' // EXFMT // ')',
     :              IOSTAT=LSTAT) VALUEI

                  IF (LSTAT .NE. IOOK) THEN
                     CALL CAT_EGT0C (GI, VALUE, NULFLG, STATUS)
                  END IF

               END IF

*
*          Real...
*          (If the column is an angle it will be converted to
*          sexagesimal format if required).

            ELSE IF (DTYPE .EQ. CAT__TYPER) THEN
               CALL CAT_EGT0R (GI, VALUER, NULFLG, STATUS)

               IF (STATUS .EQ. CAT__OK  .AND.  .NOT. NULFLG) THEN
                  IF (ANGCVT) THEN
                     VALUED = DBLE(VALUER)

                     CALL CAT1_ANFMT (VALUED, SEP, PLUS, LEAD, UNIT,
     :                 NSGDIV, DECPL, VALUE, STATUS)
                  ELSE
                     WRITE(VALUE, FMT='(' // EXFMT // ')',
     :                 IOSTAT=LSTAT) VALUER

                     IF (LSTAT .NE. IOOK) THEN
                        CALL CAT_EGT0C (GI, VALUE, NULFLG, STATUS)
                     END IF
                  END IF
               END IF

*
*          Double precision...
*          (If the column is an angle it will be converted to
*          sexagesimal format if required).

            ELSE IF (DTYPE .EQ. CAT__TYPED) THEN
               CALL CAT_EGT0D (GI, VALUED, NULFLG, STATUS)

               IF (STATUS .EQ. CAT__OK  .AND.  .NOT. NULFLG) THEN
                  IF (ANGCVT) THEN
                     CALL CAT1_ANFMT (VALUED, SEP, PLUS, LEAD, UNIT,
     :                 NSGDIV, DECPL, VALUE, STATUS)
                  ELSE
C                    print3333, exfmt, valued
C3333                format(1x, 'before exfmt, valued: ', a, 1pd18.8)
                     WRITE(VALUE, FMT='(' // EXFMT // ')',
     :                 IOSTAT=LSTAT) VALUED
C                    print3334, lstat
C3334                format(1x, 'after lstat: ', i4)

                     IF (LSTAT .NE. IOOK) THEN
                        CALL CAT_EGT0C (GI, VALUE, NULFLG, STATUS)
                     END IF
                  END IF
               END IF

*
*          Logical...

            ELSE IF (DTYPE .EQ. CAT__TYPEL) THEN
               CALL CAT_EGT0L (GI, VALUEL, NULFLG, STATUS)

               IF (STATUS .EQ. CAT__OK  .AND.  .NOT. NULFLG) THEN
                  WRITE(VALUE, FMT='(' // EXFMT // ')',
     :              IOSTAT=LSTAT) VALUEL

                  IF (LSTAT .NE. IOOK) THEN
                     CALL CAT_EGT0C (GI, VALUE, NULFLG, STATUS)
                  END IF

               END IF

*
*          Character...

            ELSE IF (DTYPE .EQ. CAT__TYPEC) THEN
               CALL CAT_EGT0C (GI, VALUEC, NULFLG, STATUS)

               IF (STATUS .EQ. CAT__OK  .AND.  .NOT. NULFLG) THEN
                  WRITE(VALUE, FMT='(' // EXFMT // ')',
     :              IOSTAT=LSTAT) VALUEC

                  IF (LSTAT .NE. IOOK) THEN
                     CALL CAT_EGT0C (GI, VALUE, NULFLG, STATUS)
                  END IF

               END IF

            ELSE

*
*             The data type is invalid; set the status.

               STATUS = CAT__INVDT

            END IF

*
*          If the null flag has been set then replace the return string
*          with the appropriate null value text.  This text is '<null>'
*          if the return string contais six or more spaces; otherwise
*          it is '?'.

            IF (NULFLG) THEN
               VSIZE = CHR_SIZE (VALUE)

               IF (VSIZE .GE. 6) THEN
                  VALUE = '<null>'
               ELSE
                  VALUE = '?'
               END IF
            END IF

         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            CALL CAT1_ERREP ('CAT_EGT0F_ERR', 'CAT_EGT0F: error '/
     :        /'getting formatted value.', STATUS)
         END IF

      END IF

C     write(17, 2003) status
C2003 format(1x, 'EGT0F on exit: ', I10 )

      END
