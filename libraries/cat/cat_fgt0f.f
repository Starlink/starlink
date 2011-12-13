      SUBROUTINE CAT_FGT0F (CI, ROWNO, GI, VALUE, NULFLG, STATUS)
*+
*  Name:
*     CAT_FGT0F
*  Purpose:
*     Get the formatted value of a scalar expression or field for a given row.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_FGT0F (CI, ROWNO, GI; VALUE, NULFLG; STATUS)
*  Description:
*     Get the formatted value of a scalar expression or field for a
*     given row.  The row may be in either a catalogue, selection or
*     index.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue, selection or index identifier.  The row number,
*        ROWNO (below), refers to the row number in the catalogue,
*        selection or index, as appropriate.
*     ROWNO  =  INTEGER (Given)
*        Number of the row to be read.
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
*     Get the specified row as the current row.
*     Obtain the value of the field or expression in the current row.
*     Report any error.
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
*     10/3/95 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
*  Arguments Given:
      INTEGER
     :  CI,
     :  ROWNO,
     :  GI
*  Arguments Returned:
      CHARACTER
     :  VALUE*(*)
      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS   ! Global status.
*  Local Variables:
      CHARACTER
     :  ERRTXT*75      ! Text of error message.
      INTEGER
     :  ERRLEN         ! Length of ERRTXT (excl. trail. blanks).
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Get the specified row as the current row.

         CALL CAT_RGET (CI, ROWNO, STATUS)

*
*       Obtain the value of the field or expression in the current row.

         CALL CAT_EGT0F (GI, VALUE, NULFLG, STATUS)

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            ERRTXT = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('CAT_FGT0F: failure getting formatted '/
     :        /'value for row ', ERRTXT, ERRLEN)
            CALL CHR_PUTI (ROWNO, ERRTXT, ERRLEN)
            CALL CHR_PUTC ('.', ERRTXT, ERRLEN)

            CALL CAT1_ERREP ('CAT_FGT0F_ERR', ERRTXT(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
