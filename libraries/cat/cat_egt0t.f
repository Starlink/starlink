      SUBROUTINE CAT_EGT0B (GI, VALUE, NULFLG, STATUS)
*+
*  Name:
*     CAT_EGT0B
*  Purpose:
*     Get the value of a scalar expression, field or parameter.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_EGT0B (GI; VALUE, NULFLG; STATUS)
*  Description:
*     Get the value of a scalar expression, evaluated from the current
*     row buffer, field or parameter.
*  Arguments:
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
*     Determine the type of the identifier.
*     If the identifier is a parameter then
*       Attempt to get the value of the parameter.
*     else if the identifier is a column then
*       Attempt to get the value of the field for the column in the
*       current row.
*     else if the identifier is a vector column element then
*       Attempt to get the value of the element for the vector column
*       in the current row.
*     else if the identifier is an expression then
*       Attempt to evaluate the expression for the current row.
*     else the identifier is not one of the permitted types
*       Set the return status.
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
*     3/5/93  (ACD): Prologue only.
*     10/8/93 (ACD): Original version.
*     8/10/93 (ACD): First stable version.
*     24/1/94 (ACD): Modified error reporting.
*     4/2/94  (ACD): Added handling of vector elements.
*     10/2/94 (ACD): Tidied up comments.
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
*  Arguments Given:
      INTEGER
     :  GI
*  Arguments Returned:
      BYTE
     :  VALUE
      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  IDTYPE, ! Type of the identifier.
     :  CURROW, ! Current row number.
     :  CI,     ! Identifier of the parent catalogue (for the col. etc.)
     :  CIELM,  ! Common block array element for the catalogue.
     :  XID,    ! Parser identifier for the expression.
     :  GIB,    ! Base identifier for vector column.
     :  ELEM    ! Element in vector column.
*.

C     write(17, 2000) status
C2000 format(1x, 'EGT0B on entry: ', I10 )

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Determine the type of the identifier.

         CALL CAT_TIDTP (GI, IDTYPE, STATUS)

C        write(17, 2001) status
C2001    format(1x, 'EGT0B after TIDTP: ', I10 )

         IF (IDTYPE .EQ. CAT__QITYP) THEN

*
*          Case where the identifier is a parameter: attempt to get its
*          value.

            CALL CAT_TIQAB (GI, 'VALUE', VALUE, STATUS)

            IF (STATUS .EQ. CAT__OK) THEN
               NULFLG = .FALSE.
            ELSE
               NULFLG = .TRUE.
            END IF

         ELSE IF (IDTYPE .EQ. CAT__FITYP) THEN

*
*          Case where the identifier is a column; attempt to get the
*          value of its field in the current row.  The procedure is as
*          follows:
*
*          get the identifier of the parent catalogue to the column,
*          get this catalogue's common block array element,
*          get the current row number,
*          finally get the value of the required field.

            CALL CAT_TIDPR (GI, CI, STATUS)
            CALL CAT1_CIELM (CI, CIELM, STATUS)
            CURROW = CROW__CAT1(CIELM)

            CALL CAT1_FIOB (.TRUE., GI, 1, CURROW, VALUE, NULFLG,
     :        STATUS)

         ELSE IF (IDTYPE .EQ. CAT__FETYP) THEN

*
*          Case where the identifier is a vector column element; attempt
*          to get the value of the appropriate element in the current
*          row.  The procedure is as follows:
*
*          get the identifier for the corresponding base column,
*          get the vector element corresponding to the identifier,
*          get the identifier of the parent catalogue to the column,
*          get this catalogue's common block array element,
*          get the current row number,
*          finally get the value of the required field.

            CALL CAT_TIQAI (GI, 'BASEID', GIB, STATUS)
            CALL CAT_TIQAI (GI, 'ELEM', ELEM, STATUS)

            CALL CAT_TIDPR (GIB, CI, STATUS)
            CALL CAT1_CIELM (CI, CIELM, STATUS)
            CURROW = CROW__CAT1(CIELM)

            CALL CAT1_FIOB (.TRUE., GIB, ELEM, CURROW, VALUE, NULFLG,
     :        STATUS)

         ELSE IF (IDTYPE .EQ. CAT__EITYP) THEN

*
*          Case where the identifier is an expression; attempt to
*          evaluate it for the current row.  The procedure is as
*          follows:
*
*          get the parser identifier for the expression,
*          get the identifier of the parent catalogue to the column,
*          get this catalogue's common block array element,
*          get the current row number,
*          finally evaluate the expression.

            CALL CAT_TIQAI (GI, 'XID', XID, STATUS)
            CALL CAT_TIDPR (GI, CI, STATUS)
            CALL CAT1_CIELM (CI, CIELM, STATUS)
            CURROW = CROW__CAT1(CIELM)

            CALL ANT_SEVALB (XID, CURROW, NULFLG, VALUE, STATUS)

C           write(17, 2002) status
C2002       format(1x, 'EGT0B after SEVALB: ', I10 )

         ELSE

*
*          The identifier is neither a parameter, column or expression.
*          Set the status.

            STATUS = CAT__INVGT

         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            CALL CAT1_ERREP ('CAT_EGT0B_ERR', 'CAT_EGT0B: error '/
     :        /'getting value.', STATUS)
         END IF

      END IF

C     write(17, 2003) status
C2003 format(1x, 'EGT0B on exit: ', I10 )

      END
      SUBROUTINE CAT_EGT0C (GI, VALUE, NULFLG, STATUS)
*+
*  Name:
*     CAT_EGT0C
*  Purpose:
*     Get the value of a scalar expression, field or parameter.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_EGT0C (GI; VALUE, NULFLG; STATUS)
*  Description:
*     Get the value of a scalar expression, evaluated from the current
*     row buffer, field or parameter.
*  Arguments:
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
*     Determine the type of the identifier.
*     If the identifier is a parameter then
*       Attempt to get the value of the parameter.
*     else if the identifier is a column then
*       Attempt to get the value of the field for the column in the
*       current row.
*     else if the identifier is a vector column element then
*       Attempt to get the value of the element for the vector column
*       in the current row.
*     else if the identifier is an expression then
*       Attempt to evaluate the expression for the current row.
*     else the identifier is not one of the permitted types
*       Set the return status.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     3/5/93  (ACD): Prologue only.
*     10/8/93 (ACD): Original version.
*     8/10/93 (ACD): First stable version.
*     24/1/94 (ACD): Modified error reporting.
*     4/2/94  (ACD): Added handling of vector elements.
*     10/2/94 (ACD): Tidied up comments.
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
*  Arguments Given:
      INTEGER
     :  GI
*  Arguments Returned:
      CHARACTER*(*)
     :  VALUE
      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  IDTYPE, ! Type of the identifier.
     :  CURROW, ! Current row number.
     :  CI,     ! Identifier of the parent catalogue (for the col. etc.)
     :  CIELM,  ! Common block array element for the catalogue.
     :  XID,    ! Parser identifier for the expression.
     :  GIB,    ! Base identifier for vector column.
     :  ELEM    ! Element in vector column.
*.

C     write(17, 2000) status
C2000 format(1x, 'EGT0C on entry: ', I10 )

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Determine the type of the identifier.

         CALL CAT_TIDTP (GI, IDTYPE, STATUS)

C        write(17, 2001) status
C2001    format(1x, 'EGT0C after TIDTP: ', I10 )

         IF (IDTYPE .EQ. CAT__QITYP) THEN

*
*          Case where the identifier is a parameter: attempt to get its
*          value.

            CALL CAT_TIQAC (GI, 'VALUE', VALUE, STATUS)

            IF (STATUS .EQ. CAT__OK) THEN
               NULFLG = .FALSE.
            ELSE
               NULFLG = .TRUE.
            END IF

         ELSE IF (IDTYPE .EQ. CAT__FITYP) THEN

*
*          Case where the identifier is a column; attempt to get the
*          value of its field in the current row.  The procedure is as
*          follows:
*
*          get the identifier of the parent catalogue to the column,
*          get this catalogue's common block array element,
*          get the current row number,
*          finally get the value of the required field.

            CALL CAT_TIDPR (GI, CI, STATUS)
            CALL CAT1_CIELM (CI, CIELM, STATUS)
            CURROW = CROW__CAT1(CIELM)

            CALL CAT1_FIOC (.TRUE., GI, 1, CURROW, VALUE, NULFLG,
     :        STATUS)

         ELSE IF (IDTYPE .EQ. CAT__FETYP) THEN

*
*          Case where the identifier is a vector column element; attempt
*          to get the value of the appropriate element in the current
*          row.  The procedure is as follows:
*
*          get the identifier for the corresponding base column,
*          get the vector element corresponding to the identifier,
*          get the identifier of the parent catalogue to the column,
*          get this catalogue's common block array element,
*          get the current row number,
*          finally get the value of the required field.

            CALL CAT_TIQAI (GI, 'BASEID', GIB, STATUS)
            CALL CAT_TIQAI (GI, 'ELEM', ELEM, STATUS)

            CALL CAT_TIDPR (GIB, CI, STATUS)
            CALL CAT1_CIELM (CI, CIELM, STATUS)
            CURROW = CROW__CAT1(CIELM)

            CALL CAT1_FIOC (.TRUE., GIB, ELEM, CURROW, VALUE, NULFLG,
     :        STATUS)

         ELSE IF (IDTYPE .EQ. CAT__EITYP) THEN

*
*          Case where the identifier is an expression; attempt to
*          evaluate it for the current row.  The procedure is as
*          follows:
*
*          get the parser identifier for the expression,
*          get the identifier of the parent catalogue to the column,
*          get this catalogue's common block array element,
*          get the current row number,
*          finally evaluate the expression.

            CALL CAT_TIQAI (GI, 'XID', XID, STATUS)
            CALL CAT_TIDPR (GI, CI, STATUS)
            CALL CAT1_CIELM (CI, CIELM, STATUS)
            CURROW = CROW__CAT1(CIELM)

            CALL ANT_SEVALC (XID, CURROW, NULFLG, VALUE, STATUS)

C           write(17, 2002) status
C2002       format(1x, 'EGT0C after SEVALC: ', I10 )

         ELSE

*
*          The identifier is neither a parameter, column or expression.
*          Set the status.

            STATUS = CAT__INVGT

         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            CALL CAT1_ERREP ('CAT_EGT0C_ERR', 'CAT_EGT0C: error '/
     :        /'getting value.', STATUS)
         END IF

      END IF

C     write(17, 2003) status
C2003 format(1x, 'EGT0C on exit: ', I10 )

      END
      SUBROUTINE CAT_EGT0D (GI, VALUE, NULFLG, STATUS)
*+
*  Name:
*     CAT_EGT0D
*  Purpose:
*     Get the value of a scalar expression, field or parameter.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_EGT0D (GI; VALUE, NULFLG; STATUS)
*  Description:
*     Get the value of a scalar expression, evaluated from the current
*     row buffer, field or parameter.
*  Arguments:
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
*     Determine the type of the identifier.
*     If the identifier is a parameter then
*       Attempt to get the value of the parameter.
*     else if the identifier is a column then
*       Attempt to get the value of the field for the column in the
*       current row.
*     else if the identifier is a vector column element then
*       Attempt to get the value of the element for the vector column
*       in the current row.
*     else if the identifier is an expression then
*       Attempt to evaluate the expression for the current row.
*     else the identifier is not one of the permitted types
*       Set the return status.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     3/5/93  (ACD): Prologue only.
*     10/8/93 (ACD): Original version.
*     8/10/93 (ACD): First stable version.
*     24/1/94 (ACD): Modified error reporting.
*     4/2/94  (ACD): Added handling of vector elements.
*     10/2/94 (ACD): Tidied up comments.
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
*  Arguments Given:
      INTEGER
     :  GI
*  Arguments Returned:
      DOUBLE PRECISION
     :  VALUE
      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  IDTYPE, ! Type of the identifier.
     :  CURROW, ! Current row number.
     :  CI,     ! Identifier of the parent catalogue (for the col. etc.)
     :  CIELM,  ! Common block array element for the catalogue.
     :  XID,    ! Parser identifier for the expression.
     :  GIB,    ! Base identifier for vector column.
     :  ELEM    ! Element in vector column.
*.

C     write(17, 2000) status
C2000 format(1x, 'EGT0D on entry: ', I10 )

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Determine the type of the identifier.

         CALL CAT_TIDTP (GI, IDTYPE, STATUS)

C        write(17, 2001) status
C2001    format(1x, 'EGT0D after TIDTP: ', I10 )

         IF (IDTYPE .EQ. CAT__QITYP) THEN

*
*          Case where the identifier is a parameter: attempt to get its
*          value.

            CALL CAT_TIQAD (GI, 'VALUE', VALUE, STATUS)

            IF (STATUS .EQ. CAT__OK) THEN
               NULFLG = .FALSE.
            ELSE
               NULFLG = .TRUE.
            END IF

         ELSE IF (IDTYPE .EQ. CAT__FITYP) THEN

*
*          Case where the identifier is a column; attempt to get the
*          value of its field in the current row.  The procedure is as
*          follows:
*
*          get the identifier of the parent catalogue to the column,
*          get this catalogue's common block array element,
*          get the current row number,
*          finally get the value of the required field.

            CALL CAT_TIDPR (GI, CI, STATUS)
            CALL CAT1_CIELM (CI, CIELM, STATUS)
            CURROW = CROW__CAT1(CIELM)

            CALL CAT1_FIOD (.TRUE., GI, 1, CURROW, VALUE, NULFLG,
     :        STATUS)

         ELSE IF (IDTYPE .EQ. CAT__FETYP) THEN

*
*          Case where the identifier is a vector column element; attempt
*          to get the value of the appropriate element in the current
*          row.  The procedure is as follows:
*
*          get the identifier for the corresponding base column,
*          get the vector element corresponding to the identifier,
*          get the identifier of the parent catalogue to the column,
*          get this catalogue's common block array element,
*          get the current row number,
*          finally get the value of the required field.

            CALL CAT_TIQAI (GI, 'BASEID', GIB, STATUS)
            CALL CAT_TIQAI (GI, 'ELEM', ELEM, STATUS)

            CALL CAT_TIDPR (GIB, CI, STATUS)
            CALL CAT1_CIELM (CI, CIELM, STATUS)
            CURROW = CROW__CAT1(CIELM)

            CALL CAT1_FIOD (.TRUE., GIB, ELEM, CURROW, VALUE, NULFLG,
     :        STATUS)

         ELSE IF (IDTYPE .EQ. CAT__EITYP) THEN

*
*          Case where the identifier is an expression; attempt to
*          evaluate it for the current row.  The procedure is as
*          follows:
*
*          get the parser identifier for the expression,
*          get the identifier of the parent catalogue to the column,
*          get this catalogue's common block array element,
*          get the current row number,
*          finally evaluate the expression.

            CALL CAT_TIQAI (GI, 'XID', XID, STATUS)
            CALL CAT_TIDPR (GI, CI, STATUS)
            CALL CAT1_CIELM (CI, CIELM, STATUS)
            CURROW = CROW__CAT1(CIELM)

            CALL ANT_SEVALD (XID, CURROW, NULFLG, VALUE, STATUS)

C           write(17, 2002) status
C2002       format(1x, 'EGT0D after SEVALD: ', I10 )

         ELSE

*
*          The identifier is neither a parameter, column or expression.
*          Set the status.

            STATUS = CAT__INVGT

         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            CALL CAT1_ERREP ('CAT_EGT0D_ERR', 'CAT_EGT0D: error '/
     :        /'getting value.', STATUS)
         END IF

      END IF

C     write(17, 2003) status
C2003 format(1x, 'EGT0D on exit: ', I10 )

      END
      SUBROUTINE CAT_EGT0I (GI, VALUE, NULFLG, STATUS)
*+
*  Name:
*     CAT_EGT0I
*  Purpose:
*     Get the value of a scalar expression, field or parameter.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_EGT0I (GI; VALUE, NULFLG; STATUS)
*  Description:
*     Get the value of a scalar expression, evaluated from the current
*     row buffer, field or parameter.
*  Arguments:
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
*     Determine the type of the identifier.
*     If the identifier is a parameter then
*       Attempt to get the value of the parameter.
*     else if the identifier is a column then
*       Attempt to get the value of the field for the column in the
*       current row.
*     else if the identifier is a vector column element then
*       Attempt to get the value of the element for the vector column
*       in the current row.
*     else if the identifier is an expression then
*       Attempt to evaluate the expression for the current row.
*     else the identifier is not one of the permitted types
*       Set the return status.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     3/5/93  (ACD): Prologue only.
*     10/8/93 (ACD): Original version.
*     8/10/93 (ACD): First stable version.
*     24/1/94 (ACD): Modified error reporting.
*     4/2/94  (ACD): Added handling of vector elements.
*     10/2/94 (ACD): Tidied up comments.
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
*  Arguments Given:
      INTEGER
     :  GI
*  Arguments Returned:
      INTEGER
     :  VALUE
      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  IDTYPE, ! Type of the identifier.
     :  CURROW, ! Current row number.
     :  CI,     ! Identifier of the parent catalogue (for the col. etc.)
     :  CIELM,  ! Common block array element for the catalogue.
     :  XID,    ! Parser identifier for the expression.
     :  GIB,    ! Base identifier for vector column.
     :  ELEM    ! Element in vector column.
*.

C     write(17, 2000) status
C2000 format(1x, 'EGT0I on entry: ', I10 )

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Determine the type of the identifier.

         CALL CAT_TIDTP (GI, IDTYPE, STATUS)

C        write(17, 2001) status
C2001    format(1x, 'EGT0I after TIDTP: ', I10 )

         IF (IDTYPE .EQ. CAT__QITYP) THEN

*
*          Case where the identifier is a parameter: attempt to get its
*          value.

            CALL CAT_TIQAI (GI, 'VALUE', VALUE, STATUS)

            IF (STATUS .EQ. CAT__OK) THEN
               NULFLG = .FALSE.
            ELSE
               NULFLG = .TRUE.
            END IF

         ELSE IF (IDTYPE .EQ. CAT__FITYP) THEN

*
*          Case where the identifier is a column; attempt to get the
*          value of its field in the current row.  The procedure is as
*          follows:
*
*          get the identifier of the parent catalogue to the column,
*          get this catalogue's common block array element,
*          get the current row number,
*          finally get the value of the required field.

            CALL CAT_TIDPR (GI, CI, STATUS)
            CALL CAT1_CIELM (CI, CIELM, STATUS)
            CURROW = CROW__CAT1(CIELM)

            CALL CAT1_FIOI (.TRUE., GI, 1, CURROW, VALUE, NULFLG,
     :        STATUS)

         ELSE IF (IDTYPE .EQ. CAT__FETYP) THEN

*
*          Case where the identifier is a vector column element; attempt
*          to get the value of the appropriate element in the current
*          row.  The procedure is as follows:
*
*          get the identifier for the corresponding base column,
*          get the vector element corresponding to the identifier,
*          get the identifier of the parent catalogue to the column,
*          get this catalogue's common block array element,
*          get the current row number,
*          finally get the value of the required field.

            CALL CAT_TIQAI (GI, 'BASEID', GIB, STATUS)
            CALL CAT_TIQAI (GI, 'ELEM', ELEM, STATUS)

            CALL CAT_TIDPR (GIB, CI, STATUS)
            CALL CAT1_CIELM (CI, CIELM, STATUS)
            CURROW = CROW__CAT1(CIELM)

            CALL CAT1_FIOI (.TRUE., GIB, ELEM, CURROW, VALUE, NULFLG,
     :        STATUS)

         ELSE IF (IDTYPE .EQ. CAT__EITYP) THEN

*
*          Case where the identifier is an expression; attempt to
*          evaluate it for the current row.  The procedure is as
*          follows:
*
*          get the parser identifier for the expression,
*          get the identifier of the parent catalogue to the column,
*          get this catalogue's common block array element,
*          get the current row number,
*          finally evaluate the expression.

            CALL CAT_TIQAI (GI, 'XID', XID, STATUS)
            CALL CAT_TIDPR (GI, CI, STATUS)
            CALL CAT1_CIELM (CI, CIELM, STATUS)
            CURROW = CROW__CAT1(CIELM)

            CALL ANT_SEVALI (XID, CURROW, NULFLG, VALUE, STATUS)

C           write(17, 2002) status
C2002       format(1x, 'EGT0I after SEVALI: ', I10 )

         ELSE

*
*          The identifier is neither a parameter, column or expression.
*          Set the status.

            STATUS = CAT__INVGT

         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            CALL CAT1_ERREP ('CAT_EGT0I_ERR', 'CAT_EGT0I: error '/
     :        /'getting value.', STATUS)
         END IF

      END IF

C     write(17, 2003) status
C2003 format(1x, 'EGT0I on exit: ', I10 )

      END
      SUBROUTINE CAT_EGT0L (GI, VALUE, NULFLG, STATUS)
*+
*  Name:
*     CAT_EGT0L
*  Purpose:
*     Get the value of a scalar expression, field or parameter.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_EGT0L (GI; VALUE, NULFLG; STATUS)
*  Description:
*     Get the value of a scalar expression, evaluated from the current
*     row buffer, field or parameter.
*  Arguments:
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
*     Determine the type of the identifier.
*     If the identifier is a parameter then
*       Attempt to get the value of the parameter.
*     else if the identifier is a column then
*       Attempt to get the value of the field for the column in the
*       current row.
*     else if the identifier is a vector column element then
*       Attempt to get the value of the element for the vector column
*       in the current row.
*     else if the identifier is an expression then
*       Attempt to evaluate the expression for the current row.
*     else the identifier is not one of the permitted types
*       Set the return status.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     3/5/93  (ACD): Prologue only.
*     10/8/93 (ACD): Original version.
*     8/10/93 (ACD): First stable version.
*     24/1/94 (ACD): Modified error reporting.
*     4/2/94  (ACD): Added handling of vector elements.
*     10/2/94 (ACD): Tidied up comments.
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
*  Arguments Given:
      INTEGER
     :  GI
*  Arguments Returned:
      LOGICAL
     :  VALUE
      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  IDTYPE, ! Type of the identifier.
     :  CURROW, ! Current row number.
     :  CI,     ! Identifier of the parent catalogue (for the col. etc.)
     :  CIELM,  ! Common block array element for the catalogue.
     :  XID,    ! Parser identifier for the expression.
     :  GIB,    ! Base identifier for vector column.
     :  ELEM    ! Element in vector column.
*.

C     write(17, 2000) status
C2000 format(1x, 'EGT0L on entry: ', I10 )

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Determine the type of the identifier.

         CALL CAT_TIDTP (GI, IDTYPE, STATUS)

C        write(17, 2001) status
C2001    format(1x, 'EGT0L after TIDTP: ', I10 )

         IF (IDTYPE .EQ. CAT__QITYP) THEN

*
*          Case where the identifier is a parameter: attempt to get its
*          value.

            CALL CAT_TIQAL (GI, 'VALUE', VALUE, STATUS)

            IF (STATUS .EQ. CAT__OK) THEN
               NULFLG = .FALSE.
            ELSE
               NULFLG = .TRUE.
            END IF

         ELSE IF (IDTYPE .EQ. CAT__FITYP) THEN

*
*          Case where the identifier is a column; attempt to get the
*          value of its field in the current row.  The procedure is as
*          follows:
*
*          get the identifier of the parent catalogue to the column,
*          get this catalogue's common block array element,
*          get the current row number,
*          finally get the value of the required field.

            CALL CAT_TIDPR (GI, CI, STATUS)
            CALL CAT1_CIELM (CI, CIELM, STATUS)
            CURROW = CROW__CAT1(CIELM)

            CALL CAT1_FIOL (.TRUE., GI, 1, CURROW, VALUE, NULFLG,
     :        STATUS)

         ELSE IF (IDTYPE .EQ. CAT__FETYP) THEN

*
*          Case where the identifier is a vector column element; attempt
*          to get the value of the appropriate element in the current
*          row.  The procedure is as follows:
*
*          get the identifier for the corresponding base column,
*          get the vector element corresponding to the identifier,
*          get the identifier of the parent catalogue to the column,
*          get this catalogue's common block array element,
*          get the current row number,
*          finally get the value of the required field.

            CALL CAT_TIQAI (GI, 'BASEID', GIB, STATUS)
            CALL CAT_TIQAI (GI, 'ELEM', ELEM, STATUS)

            CALL CAT_TIDPR (GIB, CI, STATUS)
            CALL CAT1_CIELM (CI, CIELM, STATUS)
            CURROW = CROW__CAT1(CIELM)

            CALL CAT1_FIOL (.TRUE., GIB, ELEM, CURROW, VALUE, NULFLG,
     :        STATUS)

         ELSE IF (IDTYPE .EQ. CAT__EITYP) THEN

*
*          Case where the identifier is an expression; attempt to
*          evaluate it for the current row.  The procedure is as
*          follows:
*
*          get the parser identifier for the expression,
*          get the identifier of the parent catalogue to the column,
*          get this catalogue's common block array element,
*          get the current row number,
*          finally evaluate the expression.

            CALL CAT_TIQAI (GI, 'XID', XID, STATUS)
            CALL CAT_TIDPR (GI, CI, STATUS)
            CALL CAT1_CIELM (CI, CIELM, STATUS)
            CURROW = CROW__CAT1(CIELM)

            CALL ANT_SEVALL (XID, CURROW, NULFLG, VALUE, STATUS)

C           write(17, 2002) status
C2002       format(1x, 'EGT0L after SEVALL: ', I10 )

         ELSE

*
*          The identifier is neither a parameter, column or expression.
*          Set the status.

            STATUS = CAT__INVGT

         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            CALL CAT1_ERREP ('CAT_EGT0L_ERR', 'CAT_EGT0L: error '/
     :        /'getting value.', STATUS)
         END IF

      END IF

C     write(17, 2003) status
C2003 format(1x, 'EGT0L on exit: ', I10 )

      END
      SUBROUTINE CAT_EGT0R (GI, VALUE, NULFLG, STATUS)
*+
*  Name:
*     CAT_EGT0R
*  Purpose:
*     Get the value of a scalar expression, field or parameter.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_EGT0R (GI; VALUE, NULFLG; STATUS)
*  Description:
*     Get the value of a scalar expression, evaluated from the current
*     row buffer, field or parameter.
*  Arguments:
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
*     Determine the type of the identifier.
*     If the identifier is a parameter then
*       Attempt to get the value of the parameter.
*     else if the identifier is a column then
*       Attempt to get the value of the field for the column in the
*       current row.
*     else if the identifier is a vector column element then
*       Attempt to get the value of the element for the vector column
*       in the current row.
*     else if the identifier is an expression then
*       Attempt to evaluate the expression for the current row.
*     else the identifier is not one of the permitted types
*       Set the return status.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     3/5/93  (ACD): Prologue only.
*     10/8/93 (ACD): Original version.
*     8/10/93 (ACD): First stable version.
*     24/1/94 (ACD): Modified error reporting.
*     4/2/94  (ACD): Added handling of vector elements.
*     10/2/94 (ACD): Tidied up comments.
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
*  Arguments Given:
      INTEGER
     :  GI
*  Arguments Returned:
      REAL
     :  VALUE
      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  IDTYPE, ! Type of the identifier.
     :  CURROW, ! Current row number.
     :  CI,     ! Identifier of the parent catalogue (for the col. etc.)
     :  CIELM,  ! Common block array element for the catalogue.
     :  XID,    ! Parser identifier for the expression.
     :  GIB,    ! Base identifier for vector column.
     :  ELEM    ! Element in vector column.
*.

C     write(17, 2000) status
C2000 format(1x, 'EGT0R on entry: ', I10 )

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Determine the type of the identifier.

         CALL CAT_TIDTP (GI, IDTYPE, STATUS)

C        write(17, 2001) status
C2001    format(1x, 'EGT0R after TIDTP: ', I10 )

         IF (IDTYPE .EQ. CAT__QITYP) THEN

*
*          Case where the identifier is a parameter: attempt to get its
*          value.

            CALL CAT_TIQAR (GI, 'VALUE', VALUE, STATUS)

            IF (STATUS .EQ. CAT__OK) THEN
               NULFLG = .FALSE.
            ELSE
               NULFLG = .TRUE.
            END IF

         ELSE IF (IDTYPE .EQ. CAT__FITYP) THEN

*
*          Case where the identifier is a column; attempt to get the
*          value of its field in the current row.  The procedure is as
*          follows:
*
*          get the identifier of the parent catalogue to the column,
*          get this catalogue's common block array element,
*          get the current row number,
*          finally get the value of the required field.

            CALL CAT_TIDPR (GI, CI, STATUS)
            CALL CAT1_CIELM (CI, CIELM, STATUS)
            CURROW = CROW__CAT1(CIELM)

            CALL CAT1_FIOR (.TRUE., GI, 1, CURROW, VALUE, NULFLG,
     :        STATUS)

         ELSE IF (IDTYPE .EQ. CAT__FETYP) THEN

*
*          Case where the identifier is a vector column element; attempt
*          to get the value of the appropriate element in the current
*          row.  The procedure is as follows:
*
*          get the identifier for the corresponding base column,
*          get the vector element corresponding to the identifier,
*          get the identifier of the parent catalogue to the column,
*          get this catalogue's common block array element,
*          get the current row number,
*          finally get the value of the required field.

            CALL CAT_TIQAI (GI, 'BASEID', GIB, STATUS)
            CALL CAT_TIQAI (GI, 'ELEM', ELEM, STATUS)

            CALL CAT_TIDPR (GIB, CI, STATUS)
            CALL CAT1_CIELM (CI, CIELM, STATUS)
            CURROW = CROW__CAT1(CIELM)

            CALL CAT1_FIOR (.TRUE., GIB, ELEM, CURROW, VALUE, NULFLG,
     :        STATUS)

         ELSE IF (IDTYPE .EQ. CAT__EITYP) THEN

*
*          Case where the identifier is an expression; attempt to
*          evaluate it for the current row.  The procedure is as
*          follows:
*
*          get the parser identifier for the expression,
*          get the identifier of the parent catalogue to the column,
*          get this catalogue's common block array element,
*          get the current row number,
*          finally evaluate the expression.

            CALL CAT_TIQAI (GI, 'XID', XID, STATUS)
            CALL CAT_TIDPR (GI, CI, STATUS)
            CALL CAT1_CIELM (CI, CIELM, STATUS)
            CURROW = CROW__CAT1(CIELM)

            CALL ANT_SEVALR (XID, CURROW, NULFLG, VALUE, STATUS)

C           write(17, 2002) status
C2002       format(1x, 'EGT0R after SEVALR: ', I10 )

         ELSE

*
*          The identifier is neither a parameter, column or expression.
*          Set the status.

            STATUS = CAT__INVGT

         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            CALL CAT1_ERREP ('CAT_EGT0R_ERR', 'CAT_EGT0R: error '/
     :        /'getting value.', STATUS)
         END IF

      END IF

C     write(17, 2003) status
C2003 format(1x, 'EGT0R on exit: ', I10 )

      END
      SUBROUTINE CAT_EGT0W (GI, VALUE, NULFLG, STATUS)
*+
*  Name:
*     CAT_EGT0W
*  Purpose:
*     Get the value of a scalar expression, field or parameter.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_EGT0W (GI; VALUE, NULFLG; STATUS)
*  Description:
*     Get the value of a scalar expression, evaluated from the current
*     row buffer, field or parameter.
*  Arguments:
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
*     Determine the type of the identifier.
*     If the identifier is a parameter then
*       Attempt to get the value of the parameter.
*     else if the identifier is a column then
*       Attempt to get the value of the field for the column in the
*       current row.
*     else if the identifier is a vector column element then
*       Attempt to get the value of the element for the vector column
*       in the current row.
*     else if the identifier is an expression then
*       Attempt to evaluate the expression for the current row.
*     else the identifier is not one of the permitted types
*       Set the return status.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     3/5/93  (ACD): Prologue only.
*     10/8/93 (ACD): Original version.
*     8/10/93 (ACD): First stable version.
*     24/1/94 (ACD): Modified error reporting.
*     4/2/94  (ACD): Added handling of vector elements.
*     10/2/94 (ACD): Tidied up comments.
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
*  Arguments Given:
      INTEGER
     :  GI
*  Arguments Returned:
      INTEGER*2
     :  VALUE
      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  IDTYPE, ! Type of the identifier.
     :  CURROW, ! Current row number.
     :  CI,     ! Identifier of the parent catalogue (for the col. etc.)
     :  CIELM,  ! Common block array element for the catalogue.
     :  XID,    ! Parser identifier for the expression.
     :  GIB,    ! Base identifier for vector column.
     :  ELEM    ! Element in vector column.
*.

C     write(17, 2000) status
C2000 format(1x, 'EGT0W on entry: ', I10 )

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Determine the type of the identifier.

         CALL CAT_TIDTP (GI, IDTYPE, STATUS)

C        write(17, 2001) status
C2001    format(1x, 'EGT0W after TIDTP: ', I10 )

         IF (IDTYPE .EQ. CAT__QITYP) THEN

*
*          Case where the identifier is a parameter: attempt to get its
*          value.

            CALL CAT_TIQAW (GI, 'VALUE', VALUE, STATUS)

            IF (STATUS .EQ. CAT__OK) THEN
               NULFLG = .FALSE.
            ELSE
               NULFLG = .TRUE.
            END IF

         ELSE IF (IDTYPE .EQ. CAT__FITYP) THEN

*
*          Case where the identifier is a column; attempt to get the
*          value of its field in the current row.  The procedure is as
*          follows:
*
*          get the identifier of the parent catalogue to the column,
*          get this catalogue's common block array element,
*          get the current row number,
*          finally get the value of the required field.

            CALL CAT_TIDPR (GI, CI, STATUS)
            CALL CAT1_CIELM (CI, CIELM, STATUS)
            CURROW = CROW__CAT1(CIELM)

            CALL CAT1_FIOW (.TRUE., GI, 1, CURROW, VALUE, NULFLG,
     :        STATUS)

         ELSE IF (IDTYPE .EQ. CAT__FETYP) THEN

*
*          Case where the identifier is a vector column element; attempt
*          to get the value of the appropriate element in the current
*          row.  The procedure is as follows:
*
*          get the identifier for the corresponding base column,
*          get the vector element corresponding to the identifier,
*          get the identifier of the parent catalogue to the column,
*          get this catalogue's common block array element,
*          get the current row number,
*          finally get the value of the required field.

            CALL CAT_TIQAI (GI, 'BASEID', GIB, STATUS)
            CALL CAT_TIQAI (GI, 'ELEM', ELEM, STATUS)

            CALL CAT_TIDPR (GIB, CI, STATUS)
            CALL CAT1_CIELM (CI, CIELM, STATUS)
            CURROW = CROW__CAT1(CIELM)

            CALL CAT1_FIOW (.TRUE., GIB, ELEM, CURROW, VALUE, NULFLG,
     :        STATUS)

         ELSE IF (IDTYPE .EQ. CAT__EITYP) THEN

*
*          Case where the identifier is an expression; attempt to
*          evaluate it for the current row.  The procedure is as
*          follows:
*
*          get the parser identifier for the expression,
*          get the identifier of the parent catalogue to the column,
*          get this catalogue's common block array element,
*          get the current row number,
*          finally evaluate the expression.

            CALL CAT_TIQAI (GI, 'XID', XID, STATUS)
            CALL CAT_TIDPR (GI, CI, STATUS)
            CALL CAT1_CIELM (CI, CIELM, STATUS)
            CURROW = CROW__CAT1(CIELM)

            CALL ANT_SEVALW (XID, CURROW, NULFLG, VALUE, STATUS)

C           write(17, 2002) status
C2002       format(1x, 'EGT0W after SEVALW: ', I10 )

         ELSE

*
*          The identifier is neither a parameter, column or expression.
*          Set the status.

            STATUS = CAT__INVGT

         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            CALL CAT1_ERREP ('CAT_EGT0W_ERR', 'CAT_EGT0W: error '/
     :        /'getting value.', STATUS)
         END IF

      END IF

C     write(17, 2003) status
C2003 format(1x, 'EGT0W on exit: ', I10 )

      END
