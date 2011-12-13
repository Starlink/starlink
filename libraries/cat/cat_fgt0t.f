      SUBROUTINE CAT_FGT0B (CI, ROWNO, GI, VALUE, NULFLG, STATUS)
*+
*  Name:
*     CAT_FGT0B
*  Purpose:
*     Get the value of a scalar expression or field for a given row.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_FGT0B (CI, ROWNO, GI; VALUE, NULFLG; STATUS)
*  Description:
*     Get the value of a scalar expression or field for a given row.
*     The row may be in either a catalogue, selection or index.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue, selection or index identifier.  The row number,
*        ROWNO (below), refers to the row number in the catalogue,
*        selection or index, as appropriate.
*     ROWNO  =  INTEGER (Given)
*        Number of the row to be read.
*     GI  =  INTEGER (Given)
*        Identifier for either an expression, field or parameter.
*     VALUE  =  BYTE (Returned)
*        Value to which the expression evaluates for the current row
*        buffer.
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
      BYTE
     :  VALUE
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

         CALL CAT_EGT0B (GI, VALUE, NULFLG, STATUS)

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            ERRTXT = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('CAT_FGT0B: failure getting value for '/
     :        /'row ', ERRTXT, ERRLEN)
            CALL CHR_PUTI (ROWNO, ERRTXT, ERRLEN)
            CALL CHR_PUTC ('.', ERRTXT, ERRLEN)

            CALL CAT1_ERREP ('CAT_FGT0B_ERR', ERRTXT(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT_FGT0C (CI, ROWNO, GI, VALUE, NULFLG, STATUS)
*+
*  Name:
*     CAT_FGT0C
*  Purpose:
*     Get the value of a scalar expression or field for a given row.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_FGT0C (CI, ROWNO, GI; VALUE, NULFLG; STATUS)
*  Description:
*     Get the value of a scalar expression or field for a given row.
*     The row may be in either a catalogue, selection or index.
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
*        buffer.
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
      CHARACTER*(*)
     :  VALUE
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

         CALL CAT_EGT0C (GI, VALUE, NULFLG, STATUS)

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            ERRTXT = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('CAT_FGT0C: failure getting value for '/
     :        /'row ', ERRTXT, ERRLEN)
            CALL CHR_PUTI (ROWNO, ERRTXT, ERRLEN)
            CALL CHR_PUTC ('.', ERRTXT, ERRLEN)

            CALL CAT1_ERREP ('CAT_FGT0C_ERR', ERRTXT(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT_FGT0D (CI, ROWNO, GI, VALUE, NULFLG, STATUS)
*+
*  Name:
*     CAT_FGT0D
*  Purpose:
*     Get the value of a scalar expression or field for a given row.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_FGT0D (CI, ROWNO, GI; VALUE, NULFLG; STATUS)
*  Description:
*     Get the value of a scalar expression or field for a given row.
*     The row may be in either a catalogue, selection or index.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue, selection or index identifier.  The row number,
*        ROWNO (below), refers to the row number in the catalogue,
*        selection or index, as appropriate.
*     ROWNO  =  INTEGER (Given)
*        Number of the row to be read.
*     GI  =  INTEGER (Given)
*        Identifier for either an expression, field or parameter.
*     VALUE  =  DOUBLE PRECISION (Returned)
*        Value to which the expression evaluates for the current row
*        buffer.
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
      DOUBLE PRECISION
     :  VALUE
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

         CALL CAT_EGT0D (GI, VALUE, NULFLG, STATUS)

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            ERRTXT = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('CAT_FGT0D: failure getting value for '/
     :        /'row ', ERRTXT, ERRLEN)
            CALL CHR_PUTI (ROWNO, ERRTXT, ERRLEN)
            CALL CHR_PUTC ('.', ERRTXT, ERRLEN)

            CALL CAT1_ERREP ('CAT_FGT0D_ERR', ERRTXT(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT_FGT0I (CI, ROWNO, GI, VALUE, NULFLG, STATUS)
*+
*  Name:
*     CAT_FGT0I
*  Purpose:
*     Get the value of a scalar expression or field for a given row.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_FGT0I (CI, ROWNO, GI; VALUE, NULFLG; STATUS)
*  Description:
*     Get the value of a scalar expression or field for a given row.
*     The row may be in either a catalogue, selection or index.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue, selection or index identifier.  The row number,
*        ROWNO (below), refers to the row number in the catalogue,
*        selection or index, as appropriate.
*     ROWNO  =  INTEGER (Given)
*        Number of the row to be read.
*     GI  =  INTEGER (Given)
*        Identifier for either an expression, field or parameter.
*     VALUE  =  INTEGER (Returned)
*        Value to which the expression evaluates for the current row
*        buffer.
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
      INTEGER
     :  VALUE
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

         CALL CAT_EGT0I (GI, VALUE, NULFLG, STATUS)

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            ERRTXT = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('CAT_FGT0I: failure getting value for '/
     :        /'row ', ERRTXT, ERRLEN)
            CALL CHR_PUTI (ROWNO, ERRTXT, ERRLEN)
            CALL CHR_PUTC ('.', ERRTXT, ERRLEN)

            CALL CAT1_ERREP ('CAT_FGT0I_ERR', ERRTXT(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT_FGT0L (CI, ROWNO, GI, VALUE, NULFLG, STATUS)
*+
*  Name:
*     CAT_FGT0L
*  Purpose:
*     Get the value of a scalar expression or field for a given row.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_FGT0L (CI, ROWNO, GI; VALUE, NULFLG; STATUS)
*  Description:
*     Get the value of a scalar expression or field for a given row.
*     The row may be in either a catalogue, selection or index.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue, selection or index identifier.  The row number,
*        ROWNO (below), refers to the row number in the catalogue,
*        selection or index, as appropriate.
*     ROWNO  =  INTEGER (Given)
*        Number of the row to be read.
*     GI  =  INTEGER (Given)
*        Identifier for either an expression, field or parameter.
*     VALUE  =  LOGICAL (Returned)
*        Value to which the expression evaluates for the current row
*        buffer.
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
      LOGICAL
     :  VALUE
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

         CALL CAT_EGT0L (GI, VALUE, NULFLG, STATUS)

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            ERRTXT = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('CAT_FGT0L: failure getting value for '/
     :        /'row ', ERRTXT, ERRLEN)
            CALL CHR_PUTI (ROWNO, ERRTXT, ERRLEN)
            CALL CHR_PUTC ('.', ERRTXT, ERRLEN)

            CALL CAT1_ERREP ('CAT_FGT0L_ERR', ERRTXT(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT_FGT0R (CI, ROWNO, GI, VALUE, NULFLG, STATUS)
*+
*  Name:
*     CAT_FGT0R
*  Purpose:
*     Get the value of a scalar expression or field for a given row.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_FGT0R (CI, ROWNO, GI; VALUE, NULFLG; STATUS)
*  Description:
*     Get the value of a scalar expression or field for a given row.
*     The row may be in either a catalogue, selection or index.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue, selection or index identifier.  The row number,
*        ROWNO (below), refers to the row number in the catalogue,
*        selection or index, as appropriate.
*     ROWNO  =  INTEGER (Given)
*        Number of the row to be read.
*     GI  =  INTEGER (Given)
*        Identifier for either an expression, field or parameter.
*     VALUE  =  REAL (Returned)
*        Value to which the expression evaluates for the current row
*        buffer.
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
      REAL
     :  VALUE
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

         CALL CAT_EGT0R (GI, VALUE, NULFLG, STATUS)

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            ERRTXT = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('CAT_FGT0R: failure getting value for '/
     :        /'row ', ERRTXT, ERRLEN)
            CALL CHR_PUTI (ROWNO, ERRTXT, ERRLEN)
            CALL CHR_PUTC ('.', ERRTXT, ERRLEN)

            CALL CAT1_ERREP ('CAT_FGT0R_ERR', ERRTXT(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT_FGT0W (CI, ROWNO, GI, VALUE, NULFLG, STATUS)
*+
*  Name:
*     CAT_FGT0W
*  Purpose:
*     Get the value of a scalar expression or field for a given row.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_FGT0W (CI, ROWNO, GI; VALUE, NULFLG; STATUS)
*  Description:
*     Get the value of a scalar expression or field for a given row.
*     The row may be in either a catalogue, selection or index.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue, selection or index identifier.  The row number,
*        ROWNO (below), refers to the row number in the catalogue,
*        selection or index, as appropriate.
*     ROWNO  =  INTEGER (Given)
*        Number of the row to be read.
*     GI  =  INTEGER (Given)
*        Identifier for either an expression, field or parameter.
*     VALUE  =  INTEGER*2 (Returned)
*        Value to which the expression evaluates for the current row
*        buffer.
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
      INTEGER*2
     :  VALUE
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

         CALL CAT_EGT0W (GI, VALUE, NULFLG, STATUS)

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            ERRTXT = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('CAT_FGT0W: failure getting value for '/
     :        /'row ', ERRTXT, ERRLEN)
            CALL CHR_PUTI (ROWNO, ERRTXT, ERRLEN)
            CALL CHR_PUTC ('.', ERRTXT, ERRLEN)

            CALL CAT1_ERREP ('CAT_FGT0W_ERR', ERRTXT(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
