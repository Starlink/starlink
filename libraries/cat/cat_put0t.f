      SUBROUTINE CAT_PUT0B (PI, VALUE, NULFLG, STATUS)
*+
*  Name:
*     CAT_PUT0B
*  Purpose:
*     Put a value to a scalar part (field or column).
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_PUT0B (PI, VALUE, NULFLG; STATUS)
*  Description:
*     Put a value to a scalar part (field or column). If the identifier
*     refers to a field the value is written to the current row buffer.
*  Arguments:
*     PI  =  INTEGER (Given)
*        Part identifier.
*     VALUE  =  BYTE (Given)
*        Value to PUT to the part.
*     NULFLG  =  LOGICAL (Given)
*        A flag indicating whether or not the value is null or not:
*        .TRUE.  - The value is null,
*        .FALSE. - The value is not null.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Obtain the identifier for the parent catalogue.
*     Obtain the access mode of the catalogue.
*     If the catalogue has been opened for 'WRITE' access then
*       If the initial creation of the columns and parameters is
*       not finished then
*         Finish the initial creation of the columns and parameters.
*       end if
*       Determine whether the part is a column or parameter.
*       If the part is a column then
*         Write the value to the column's field in the current row.
*       else the part is a vector column element then
*         Determine the column to which the part refers.
*         Determine the element number to which the part refers.
*         Write the value to the appropriate vector column element in
*         the current row.
*       else the part is a parameter
*         Write the value to the parameter.
*       else
*         Set the status.
*       end if
*     else
*       Set the status.
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
*     3/5/93   (ACD): Prologue only.
*     13/8/93  (ACD): Original version.
*     11/10/93 (ACD): First stable version.
*     24/1/94  (ACD): Modified error reporting.
*     2/2/94   (ACD): Added handling vector column element.
*     10/6/98  (ACD): Improved the error reporting.
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
      INCLUDE 'CAT1_CATS_CMN'     ! Catalogues common block.
*  Arguments Given:
      INTEGER
     :  PI
      BYTE
     :  VALUE
      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  CI,      ! Identifier to the part's parent catalogue.
     :  CIELM,   ! Common block array element for the parent catalogue.
     :  MODE,    ! Access mode of the parent catalogue.
     :  IDTYPE,  ! Type of the part (column, parameter etc).
     :  CURROW,  ! Current row number.
     :  PIB,     ! Base identifier for vector column.
     :  ELEM,    ! Element in vector column.
     :  ERRLEN,  ! Length of ERRMSG (excl. trail. blanks).
     :  LCMPNM,  !   "    "  CMPNAM ( "  .   "  .   "   ).
     :  ESTAT    ! Status generating error message.
      CHARACTER
     :  ERRMSG*75,           ! Error message.
     :  CMPNAM*(CAT__SZCMP)  ! Component name.
*.

      IF (STATUS .EQ. CAT__OK) THEN
         CURROW = 0
         IDTYPE = 0

*
*       Obtain the identifier for the part's parent catalogue and the
*       corresponding common block array element.

         CALL CAT_TIDPR (PI, CI, STATUS)
         CALL CAT1_CIELM (CI, CIELM, STATUS)

*
*       Obtain the access mode for the catalogue.

         MODE = MODE__CAT1(CIELM)

*
*       Proceed if the catalogue is opened for write.

         IF (MODE .EQ. CAT1__MDWRT) THEN

*
*          Determine whether the part is a column or parameter.

            CALL CAT_TIDTP (PI, IDTYPE, STATUS)

*
*          If the initial creation of the columns and parameters is
*          not finished then finish the creation.

            IF (.NOT. FINSH__CAT1(CIELM) ) THEN
               CALL CAT1_FINCR (CI, STATUS)
               FINSH__CAT1(CIELM) = .TRUE.
            END IF

*
*          Write the value in an appropriate fashion, depending on
*          whether the part is a column, vector column element or
*          parameter.

            IF (IDTYPE .EQ. CAT__FITYP) THEN

*
*             The part is a column.

               CURROW = CROW__CAT1(CIELM)

               CALL CAT1_FIOB (.FALSE., PI, 1, CURROW, VALUE, NULFLG,
     :           STATUS)

            ELSE IF (IDTYPE .EQ. CAT__FETYP) THEN

*
*             The part is a vector column element.

               CURROW = CROW__CAT1(CIELM)

               CALL CAT_TIQAI (PI, 'BASEID', PIB, STATUS)
               CALL CAT_TIQAI (PI, 'ELEM', ELEM, STATUS)

               CALL CAT1_FIOB (.FALSE., PIB, ELEM, CURROW, VALUE,
     :           NULFLG, STATUS)

            ELSE IF (IDTYPE .EQ. CAT__QITYP) THEN

*
*             The part is a parameter.

               CALL CAT_TATTB (PI, 'VALUE', VALUE, STATUS)

            ELSE
               STATUS = CAT__INVPT

            END IF

         ELSE
            STATUS = CAT__INVWT

         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            ERRMSG = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('CAT_PUT0B: error putting value '/
     :        /'for component ', ERRMSG, ERRLEN)

            ESTAT = CAT__OK
            CALL CAT_TIQAC (PI, 'NAME', CMPNAM, ESTAT)

            IF (CMPNAM .NE. ' ') THEN
               LCMPNM = CHR_LEN(CMPNAM)
               CALL CHR_PUTC (CMPNAM(1 : LCMPNM), ERRMSG, ERRLEN)
            ELSE
               CALL CHR_PUTC ('<Unknown>', ERRMSG, ERRLEN)
            END IF

            IF (IDTYPE .EQ. CAT__FITYP  .OR.  IDTYPE .EQ. CAT__FETYP)
     :        THEN
               CALL CHR_PUTC (', row ', ERRMSG, ERRLEN)
               CALL CHR_PUTI (CURROW, ERRMSG, ERRLEN)
            END IF

            CALL CHR_PUTC ('.', ERRMSG, ERRLEN)

            CALL CAT1_ERREP ('CAT_PUT0B_ERR', ERRMSG(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT_PUT0C (PI, VALUE, NULFLG, STATUS)
*+
*  Name:
*     CAT_PUT0C
*  Purpose:
*     Put a value to a scalar part (field or column).
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_PUT0C (PI, VALUE, NULFLG; STATUS)
*  Description:
*     Put a value to a scalar part (field or column). If the identifier
*     refers to a field the value is written to the current row buffer.
*  Arguments:
*     PI  =  INTEGER (Given)
*        Part identifier.
*     VALUE  =  CHARACTER*(*) (Given)
*        Value to PUT to the part.
*     NULFLG  =  LOGICAL (Given)
*        A flag indicating whether or not the value is null or not:
*        .TRUE.  - The value is null,
*        .FALSE. - The value is not null.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Obtain the identifier for the parent catalogue.
*     Obtain the access mode of the catalogue.
*     If the catalogue has been opened for 'WRITE' access then
*       If the initial creation of the columns and parameters is
*       not finished then
*         Finish the initial creation of the columns and parameters.
*       end if
*       Determine whether the part is a column or parameter.
*       If the part is a column then
*         Write the value to the column's field in the current row.
*       else the part is a vector column element then
*         Determine the column to which the part refers.
*         Determine the element number to which the part refers.
*         Write the value to the appropriate vector column element in
*         the current row.
*       else the part is a parameter
*         Write the value to the parameter.
*       else
*         Set the status.
*       end if
*     else
*       Set the status.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     3/5/93   (ACD): Prologue only.
*     13/8/93  (ACD): Original version.
*     11/10/93 (ACD): First stable version.
*     24/1/94  (ACD): Modified error reporting.
*     2/2/94   (ACD): Added handling vector column element.
*     10/6/98  (ACD): Improved the error reporting.
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
      INCLUDE 'CAT1_CATS_CMN'     ! Catalogues common block.
*  Arguments Given:
      INTEGER
     :  PI
      CHARACTER*(*)
     :  VALUE
      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  CI,      ! Identifier to the part's parent catalogue.
     :  CIELM,   ! Common block array element for the parent catalogue.
     :  MODE,    ! Access mode of the parent catalogue.
     :  IDTYPE,  ! Type of the part (column, parameter etc).
     :  CURROW,  ! Current row number.
     :  PIB,     ! Base identifier for vector column.
     :  ELEM,    ! Element in vector column.
     :  ERRLEN,  ! Length of ERRMSG (excl. trail. blanks).
     :  LCMPNM,  !   "    "  CMPNAM ( "  .   "  .   "   ).
     :  ESTAT    ! Status generating error message.
      CHARACTER
     :  ERRMSG*75,           ! Error message.
     :  CMPNAM*(CAT__SZCMP)  ! Component name.
*.

      IF (STATUS .EQ. CAT__OK) THEN
         CURROW = 0
         IDTYPE = 0

*
*       Obtain the identifier for the part's parent catalogue and the
*       corresponding common block array element.

         CALL CAT_TIDPR (PI, CI, STATUS)
         CALL CAT1_CIELM (CI, CIELM, STATUS)

*
*       Obtain the access mode for the catalogue.

         MODE = MODE__CAT1(CIELM)

*
*       Proceed if the catalogue is opened for write.

         IF (MODE .EQ. CAT1__MDWRT) THEN

*
*          Determine whether the part is a column or parameter.

            CALL CAT_TIDTP (PI, IDTYPE, STATUS)

*
*          If the initial creation of the columns and parameters is
*          not finished then finish the creation.

            IF (.NOT. FINSH__CAT1(CIELM) ) THEN
               CALL CAT1_FINCR (CI, STATUS)
               FINSH__CAT1(CIELM) = .TRUE.
            END IF

*
*          Write the value in an appropriate fashion, depending on
*          whether the part is a column, vector column element or
*          parameter.

            IF (IDTYPE .EQ. CAT__FITYP) THEN

*
*             The part is a column.

               CURROW = CROW__CAT1(CIELM)

               CALL CAT1_FIOC (.FALSE., PI, 1, CURROW, VALUE, NULFLG,
     :           STATUS)

            ELSE IF (IDTYPE .EQ. CAT__FETYP) THEN

*
*             The part is a vector column element.

               CURROW = CROW__CAT1(CIELM)

               CALL CAT_TIQAI (PI, 'BASEID', PIB, STATUS)
               CALL CAT_TIQAI (PI, 'ELEM', ELEM, STATUS)

               CALL CAT1_FIOC (.FALSE., PIB, ELEM, CURROW, VALUE,
     :           NULFLG, STATUS)

            ELSE IF (IDTYPE .EQ. CAT__QITYP) THEN

*
*             The part is a parameter.

               CALL CAT_TATTC (PI, 'VALUE', VALUE, STATUS)

            ELSE
               STATUS = CAT__INVPT

            END IF

         ELSE
            STATUS = CAT__INVWT

         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            ERRMSG = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('CAT_PUT0C: error putting value '/
     :        /'for component ', ERRMSG, ERRLEN)

            ESTAT = CAT__OK
            CALL CAT_TIQAC (PI, 'NAME', CMPNAM, ESTAT)

            IF (CMPNAM .NE. ' ') THEN
               LCMPNM = CHR_LEN(CMPNAM)
               CALL CHR_PUTC (CMPNAM(1 : LCMPNM), ERRMSG, ERRLEN)
            ELSE
               CALL CHR_PUTC ('<Unknown>', ERRMSG, ERRLEN)
            END IF

            IF (IDTYPE .EQ. CAT__FITYP  .OR.  IDTYPE .EQ. CAT__FETYP)
     :        THEN
               CALL CHR_PUTC (', row ', ERRMSG, ERRLEN)
               CALL CHR_PUTI (CURROW, ERRMSG, ERRLEN)
            END IF

            CALL CHR_PUTC ('.', ERRMSG, ERRLEN)

            CALL CAT1_ERREP ('CAT_PUT0C_ERR', ERRMSG(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT_PUT0D (PI, VALUE, NULFLG, STATUS)
*+
*  Name:
*     CAT_PUT0D
*  Purpose:
*     Put a value to a scalar part (field or column).
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_PUT0D (PI, VALUE, NULFLG; STATUS)
*  Description:
*     Put a value to a scalar part (field or column). If the identifier
*     refers to a field the value is written to the current row buffer.
*  Arguments:
*     PI  =  INTEGER (Given)
*        Part identifier.
*     VALUE  =  DOUBLE PRECISION (Given)
*        Value to PUT to the part.
*     NULFLG  =  LOGICAL (Given)
*        A flag indicating whether or not the value is null or not:
*        .TRUE.  - The value is null,
*        .FALSE. - The value is not null.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Obtain the identifier for the parent catalogue.
*     Obtain the access mode of the catalogue.
*     If the catalogue has been opened for 'WRITE' access then
*       If the initial creation of the columns and parameters is
*       not finished then
*         Finish the initial creation of the columns and parameters.
*       end if
*       Determine whether the part is a column or parameter.
*       If the part is a column then
*         Write the value to the column's field in the current row.
*       else the part is a vector column element then
*         Determine the column to which the part refers.
*         Determine the element number to which the part refers.
*         Write the value to the appropriate vector column element in
*         the current row.
*       else the part is a parameter
*         Write the value to the parameter.
*       else
*         Set the status.
*       end if
*     else
*       Set the status.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     3/5/93   (ACD): Prologue only.
*     13/8/93  (ACD): Original version.
*     11/10/93 (ACD): First stable version.
*     24/1/94  (ACD): Modified error reporting.
*     2/2/94   (ACD): Added handling vector column element.
*     10/6/98  (ACD): Improved the error reporting.
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
      INCLUDE 'CAT1_CATS_CMN'     ! Catalogues common block.
*  Arguments Given:
      INTEGER
     :  PI
      DOUBLE PRECISION
     :  VALUE
      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  CI,      ! Identifier to the part's parent catalogue.
     :  CIELM,   ! Common block array element for the parent catalogue.
     :  MODE,    ! Access mode of the parent catalogue.
     :  IDTYPE,  ! Type of the part (column, parameter etc).
     :  CURROW,  ! Current row number.
     :  PIB,     ! Base identifier for vector column.
     :  ELEM,    ! Element in vector column.
     :  ERRLEN,  ! Length of ERRMSG (excl. trail. blanks).
     :  LCMPNM,  !   "    "  CMPNAM ( "  .   "  .   "   ).
     :  ESTAT    ! Status generating error message.
      CHARACTER
     :  ERRMSG*75,           ! Error message.
     :  CMPNAM*(CAT__SZCMP)  ! Component name.
*.

      IF (STATUS .EQ. CAT__OK) THEN
         CURROW = 0
         IDTYPE = 0

*
*       Obtain the identifier for the part's parent catalogue and the
*       corresponding common block array element.

         CALL CAT_TIDPR (PI, CI, STATUS)
         CALL CAT1_CIELM (CI, CIELM, STATUS)

*
*       Obtain the access mode for the catalogue.

         MODE = MODE__CAT1(CIELM)

*
*       Proceed if the catalogue is opened for write.

         IF (MODE .EQ. CAT1__MDWRT) THEN

*
*          Determine whether the part is a column or parameter.

            CALL CAT_TIDTP (PI, IDTYPE, STATUS)

*
*          If the initial creation of the columns and parameters is
*          not finished then finish the creation.

            IF (.NOT. FINSH__CAT1(CIELM) ) THEN
               CALL CAT1_FINCR (CI, STATUS)
               FINSH__CAT1(CIELM) = .TRUE.
            END IF

*
*          Write the value in an appropriate fashion, depending on
*          whether the part is a column, vector column element or
*          parameter.

            IF (IDTYPE .EQ. CAT__FITYP) THEN

*
*             The part is a column.

               CURROW = CROW__CAT1(CIELM)

               CALL CAT1_FIOD (.FALSE., PI, 1, CURROW, VALUE, NULFLG,
     :           STATUS)

            ELSE IF (IDTYPE .EQ. CAT__FETYP) THEN

*
*             The part is a vector column element.

               CURROW = CROW__CAT1(CIELM)

               CALL CAT_TIQAI (PI, 'BASEID', PIB, STATUS)
               CALL CAT_TIQAI (PI, 'ELEM', ELEM, STATUS)

               CALL CAT1_FIOD (.FALSE., PIB, ELEM, CURROW, VALUE,
     :           NULFLG, STATUS)

            ELSE IF (IDTYPE .EQ. CAT__QITYP) THEN

*
*             The part is a parameter.

               CALL CAT_TATTD (PI, 'VALUE', VALUE, STATUS)

            ELSE
               STATUS = CAT__INVPT

            END IF

         ELSE
            STATUS = CAT__INVWT

         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            ERRMSG = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('CAT_PUT0D: error putting value '/
     :        /'for component ', ERRMSG, ERRLEN)

            ESTAT = CAT__OK
            CALL CAT_TIQAC (PI, 'NAME', CMPNAM, ESTAT)

            IF (CMPNAM .NE. ' ') THEN
               LCMPNM = CHR_LEN(CMPNAM)
               CALL CHR_PUTC (CMPNAM(1 : LCMPNM), ERRMSG, ERRLEN)
            ELSE
               CALL CHR_PUTC ('<Unknown>', ERRMSG, ERRLEN)
            END IF

            IF (IDTYPE .EQ. CAT__FITYP  .OR.  IDTYPE .EQ. CAT__FETYP)
     :        THEN
               CALL CHR_PUTC (', row ', ERRMSG, ERRLEN)
               CALL CHR_PUTI (CURROW, ERRMSG, ERRLEN)
            END IF

            CALL CHR_PUTC ('.', ERRMSG, ERRLEN)

            CALL CAT1_ERREP ('CAT_PUT0D_ERR', ERRMSG(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT_PUT0I (PI, VALUE, NULFLG, STATUS)
*+
*  Name:
*     CAT_PUT0I
*  Purpose:
*     Put a value to a scalar part (field or column).
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_PUT0I (PI, VALUE, NULFLG; STATUS)
*  Description:
*     Put a value to a scalar part (field or column). If the identifier
*     refers to a field the value is written to the current row buffer.
*  Arguments:
*     PI  =  INTEGER (Given)
*        Part identifier.
*     VALUE  =  INTEGER (Given)
*        Value to PUT to the part.
*     NULFLG  =  LOGICAL (Given)
*        A flag indicating whether or not the value is null or not:
*        .TRUE.  - The value is null,
*        .FALSE. - The value is not null.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Obtain the identifier for the parent catalogue.
*     Obtain the access mode of the catalogue.
*     If the catalogue has been opened for 'WRITE' access then
*       If the initial creation of the columns and parameters is
*       not finished then
*         Finish the initial creation of the columns and parameters.
*       end if
*       Determine whether the part is a column or parameter.
*       If the part is a column then
*         Write the value to the column's field in the current row.
*       else the part is a vector column element then
*         Determine the column to which the part refers.
*         Determine the element number to which the part refers.
*         Write the value to the appropriate vector column element in
*         the current row.
*       else the part is a parameter
*         Write the value to the parameter.
*       else
*         Set the status.
*       end if
*     else
*       Set the status.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     3/5/93   (ACD): Prologue only.
*     13/8/93  (ACD): Original version.
*     11/10/93 (ACD): First stable version.
*     24/1/94  (ACD): Modified error reporting.
*     2/2/94   (ACD): Added handling vector column element.
*     10/6/98  (ACD): Improved the error reporting.
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
      INCLUDE 'CAT1_CATS_CMN'     ! Catalogues common block.
*  Arguments Given:
      INTEGER
     :  PI
      INTEGER
     :  VALUE
      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  CI,      ! Identifier to the part's parent catalogue.
     :  CIELM,   ! Common block array element for the parent catalogue.
     :  MODE,    ! Access mode of the parent catalogue.
     :  IDTYPE,  ! Type of the part (column, parameter etc).
     :  CURROW,  ! Current row number.
     :  PIB,     ! Base identifier for vector column.
     :  ELEM,    ! Element in vector column.
     :  ERRLEN,  ! Length of ERRMSG (excl. trail. blanks).
     :  LCMPNM,  !   "    "  CMPNAM ( "  .   "  .   "   ).
     :  ESTAT    ! Status generating error message.
      CHARACTER
     :  ERRMSG*75,           ! Error message.
     :  CMPNAM*(CAT__SZCMP)  ! Component name.
*.

      IF (STATUS .EQ. CAT__OK) THEN
         CURROW = 0
         IDTYPE = 0

*
*       Obtain the identifier for the part's parent catalogue and the
*       corresponding common block array element.

         CALL CAT_TIDPR (PI, CI, STATUS)
         CALL CAT1_CIELM (CI, CIELM, STATUS)

*
*       Obtain the access mode for the catalogue.

         MODE = MODE__CAT1(CIELM)

*
*       Proceed if the catalogue is opened for write.

         IF (MODE .EQ. CAT1__MDWRT) THEN

*
*          Determine whether the part is a column or parameter.

            CALL CAT_TIDTP (PI, IDTYPE, STATUS)

*
*          If the initial creation of the columns and parameters is
*          not finished then finish the creation.

            IF (.NOT. FINSH__CAT1(CIELM) ) THEN
               CALL CAT1_FINCR (CI, STATUS)
               FINSH__CAT1(CIELM) = .TRUE.
            END IF

*
*          Write the value in an appropriate fashion, depending on
*          whether the part is a column, vector column element or
*          parameter.

            IF (IDTYPE .EQ. CAT__FITYP) THEN

*
*             The part is a column.

               CURROW = CROW__CAT1(CIELM)

               CALL CAT1_FIOI (.FALSE., PI, 1, CURROW, VALUE, NULFLG,
     :           STATUS)

            ELSE IF (IDTYPE .EQ. CAT__FETYP) THEN

*
*             The part is a vector column element.

               CURROW = CROW__CAT1(CIELM)

               CALL CAT_TIQAI (PI, 'BASEID', PIB, STATUS)
               CALL CAT_TIQAI (PI, 'ELEM', ELEM, STATUS)

               CALL CAT1_FIOI (.FALSE., PIB, ELEM, CURROW, VALUE,
     :           NULFLG, STATUS)

            ELSE IF (IDTYPE .EQ. CAT__QITYP) THEN

*
*             The part is a parameter.

               CALL CAT_TATTI (PI, 'VALUE', VALUE, STATUS)

            ELSE
               STATUS = CAT__INVPT

            END IF

         ELSE
            STATUS = CAT__INVWT

         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            ERRMSG = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('CAT_PUT0I: error putting value '/
     :        /'for component ', ERRMSG, ERRLEN)

            ESTAT = CAT__OK
            CALL CAT_TIQAC (PI, 'NAME', CMPNAM, ESTAT)

            IF (CMPNAM .NE. ' ') THEN
               LCMPNM = CHR_LEN(CMPNAM)
               CALL CHR_PUTC (CMPNAM(1 : LCMPNM), ERRMSG, ERRLEN)
            ELSE
               CALL CHR_PUTC ('<Unknown>', ERRMSG, ERRLEN)
            END IF

            IF (IDTYPE .EQ. CAT__FITYP  .OR.  IDTYPE .EQ. CAT__FETYP)
     :        THEN
               CALL CHR_PUTC (', row ', ERRMSG, ERRLEN)
               CALL CHR_PUTI (CURROW, ERRMSG, ERRLEN)
            END IF

            CALL CHR_PUTC ('.', ERRMSG, ERRLEN)

            CALL CAT1_ERREP ('CAT_PUT0I_ERR', ERRMSG(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT_PUT0L (PI, VALUE, NULFLG, STATUS)
*+
*  Name:
*     CAT_PUT0L
*  Purpose:
*     Put a value to a scalar part (field or column).
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_PUT0L (PI, VALUE, NULFLG; STATUS)
*  Description:
*     Put a value to a scalar part (field or column). If the identifier
*     refers to a field the value is written to the current row buffer.
*  Arguments:
*     PI  =  INTEGER (Given)
*        Part identifier.
*     VALUE  =  LOGICAL (Given)
*        Value to PUT to the part.
*     NULFLG  =  LOGICAL (Given)
*        A flag indicating whether or not the value is null or not:
*        .TRUE.  - The value is null,
*        .FALSE. - The value is not null.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Obtain the identifier for the parent catalogue.
*     Obtain the access mode of the catalogue.
*     If the catalogue has been opened for 'WRITE' access then
*       If the initial creation of the columns and parameters is
*       not finished then
*         Finish the initial creation of the columns and parameters.
*       end if
*       Determine whether the part is a column or parameter.
*       If the part is a column then
*         Write the value to the column's field in the current row.
*       else the part is a vector column element then
*         Determine the column to which the part refers.
*         Determine the element number to which the part refers.
*         Write the value to the appropriate vector column element in
*         the current row.
*       else the part is a parameter
*         Write the value to the parameter.
*       else
*         Set the status.
*       end if
*     else
*       Set the status.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     3/5/93   (ACD): Prologue only.
*     13/8/93  (ACD): Original version.
*     11/10/93 (ACD): First stable version.
*     24/1/94  (ACD): Modified error reporting.
*     2/2/94   (ACD): Added handling vector column element.
*     10/6/98  (ACD): Improved the error reporting.
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
      INCLUDE 'CAT1_CATS_CMN'     ! Catalogues common block.
*  Arguments Given:
      INTEGER
     :  PI
      LOGICAL
     :  VALUE
      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  CI,      ! Identifier to the part's parent catalogue.
     :  CIELM,   ! Common block array element for the parent catalogue.
     :  MODE,    ! Access mode of the parent catalogue.
     :  IDTYPE,  ! Type of the part (column, parameter etc).
     :  CURROW,  ! Current row number.
     :  PIB,     ! Base identifier for vector column.
     :  ELEM,    ! Element in vector column.
     :  ERRLEN,  ! Length of ERRMSG (excl. trail. blanks).
     :  LCMPNM,  !   "    "  CMPNAM ( "  .   "  .   "   ).
     :  ESTAT    ! Status generating error message.
      CHARACTER
     :  ERRMSG*75,           ! Error message.
     :  CMPNAM*(CAT__SZCMP)  ! Component name.
*.

      IF (STATUS .EQ. CAT__OK) THEN
         CURROW = 0
         IDTYPE = 0

*
*       Obtain the identifier for the part's parent catalogue and the
*       corresponding common block array element.

         CALL CAT_TIDPR (PI, CI, STATUS)
         CALL CAT1_CIELM (CI, CIELM, STATUS)

*
*       Obtain the access mode for the catalogue.

         MODE = MODE__CAT1(CIELM)

*
*       Proceed if the catalogue is opened for write.

         IF (MODE .EQ. CAT1__MDWRT) THEN

*
*          Determine whether the part is a column or parameter.

            CALL CAT_TIDTP (PI, IDTYPE, STATUS)

*
*          If the initial creation of the columns and parameters is
*          not finished then finish the creation.

            IF (.NOT. FINSH__CAT1(CIELM) ) THEN
               CALL CAT1_FINCR (CI, STATUS)
               FINSH__CAT1(CIELM) = .TRUE.
            END IF

*
*          Write the value in an appropriate fashion, depending on
*          whether the part is a column, vector column element or
*          parameter.

            IF (IDTYPE .EQ. CAT__FITYP) THEN

*
*             The part is a column.

               CURROW = CROW__CAT1(CIELM)

               CALL CAT1_FIOL (.FALSE., PI, 1, CURROW, VALUE, NULFLG,
     :           STATUS)

            ELSE IF (IDTYPE .EQ. CAT__FETYP) THEN

*
*             The part is a vector column element.

               CURROW = CROW__CAT1(CIELM)

               CALL CAT_TIQAI (PI, 'BASEID', PIB, STATUS)
               CALL CAT_TIQAI (PI, 'ELEM', ELEM, STATUS)

               CALL CAT1_FIOL (.FALSE., PIB, ELEM, CURROW, VALUE,
     :           NULFLG, STATUS)

            ELSE IF (IDTYPE .EQ. CAT__QITYP) THEN

*
*             The part is a parameter.

               CALL CAT_TATTL (PI, 'VALUE', VALUE, STATUS)

            ELSE
               STATUS = CAT__INVPT

            END IF

         ELSE
            STATUS = CAT__INVWT

         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            ERRMSG = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('CAT_PUT0L: error putting value '/
     :        /'for component ', ERRMSG, ERRLEN)

            ESTAT = CAT__OK
            CALL CAT_TIQAC (PI, 'NAME', CMPNAM, ESTAT)

            IF (CMPNAM .NE. ' ') THEN
               LCMPNM = CHR_LEN(CMPNAM)
               CALL CHR_PUTC (CMPNAM(1 : LCMPNM), ERRMSG, ERRLEN)
            ELSE
               CALL CHR_PUTC ('<Unknown>', ERRMSG, ERRLEN)
            END IF

            IF (IDTYPE .EQ. CAT__FITYP  .OR.  IDTYPE .EQ. CAT__FETYP)
     :        THEN
               CALL CHR_PUTC (', row ', ERRMSG, ERRLEN)
               CALL CHR_PUTI (CURROW, ERRMSG, ERRLEN)
            END IF

            CALL CHR_PUTC ('.', ERRMSG, ERRLEN)

            CALL CAT1_ERREP ('CAT_PUT0L_ERR', ERRMSG(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT_PUT0R (PI, VALUE, NULFLG, STATUS)
*+
*  Name:
*     CAT_PUT0R
*  Purpose:
*     Put a value to a scalar part (field or column).
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_PUT0R (PI, VALUE, NULFLG; STATUS)
*  Description:
*     Put a value to a scalar part (field or column). If the identifier
*     refers to a field the value is written to the current row buffer.
*  Arguments:
*     PI  =  INTEGER (Given)
*        Part identifier.
*     VALUE  =  REAL (Given)
*        Value to PUT to the part.
*     NULFLG  =  LOGICAL (Given)
*        A flag indicating whether or not the value is null or not:
*        .TRUE.  - The value is null,
*        .FALSE. - The value is not null.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Obtain the identifier for the parent catalogue.
*     Obtain the access mode of the catalogue.
*     If the catalogue has been opened for 'WRITE' access then
*       If the initial creation of the columns and parameters is
*       not finished then
*         Finish the initial creation of the columns and parameters.
*       end if
*       Determine whether the part is a column or parameter.
*       If the part is a column then
*         Write the value to the column's field in the current row.
*       else the part is a vector column element then
*         Determine the column to which the part refers.
*         Determine the element number to which the part refers.
*         Write the value to the appropriate vector column element in
*         the current row.
*       else the part is a parameter
*         Write the value to the parameter.
*       else
*         Set the status.
*       end if
*     else
*       Set the status.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     3/5/93   (ACD): Prologue only.
*     13/8/93  (ACD): Original version.
*     11/10/93 (ACD): First stable version.
*     24/1/94  (ACD): Modified error reporting.
*     2/2/94   (ACD): Added handling vector column element.
*     10/6/98  (ACD): Improved the error reporting.
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
      INCLUDE 'CAT1_CATS_CMN'     ! Catalogues common block.
*  Arguments Given:
      INTEGER
     :  PI
      REAL
     :  VALUE
      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  CI,      ! Identifier to the part's parent catalogue.
     :  CIELM,   ! Common block array element for the parent catalogue.
     :  MODE,    ! Access mode of the parent catalogue.
     :  IDTYPE,  ! Type of the part (column, parameter etc).
     :  CURROW,  ! Current row number.
     :  PIB,     ! Base identifier for vector column.
     :  ELEM,    ! Element in vector column.
     :  ERRLEN,  ! Length of ERRMSG (excl. trail. blanks).
     :  LCMPNM,  !   "    "  CMPNAM ( "  .   "  .   "   ).
     :  ESTAT    ! Status generating error message.
      CHARACTER
     :  ERRMSG*75,           ! Error message.
     :  CMPNAM*(CAT__SZCMP)  ! Component name.
*.

      IF (STATUS .EQ. CAT__OK) THEN
         CURROW = 0
         IDTYPE = 0

*
*       Obtain the identifier for the part's parent catalogue and the
*       corresponding common block array element.

         CALL CAT_TIDPR (PI, CI, STATUS)
         CALL CAT1_CIELM (CI, CIELM, STATUS)

*
*       Obtain the access mode for the catalogue.

         MODE = MODE__CAT1(CIELM)

*
*       Proceed if the catalogue is opened for write.

         IF (MODE .EQ. CAT1__MDWRT) THEN

*
*          Determine whether the part is a column or parameter.

            CALL CAT_TIDTP (PI, IDTYPE, STATUS)

*
*          If the initial creation of the columns and parameters is
*          not finished then finish the creation.

            IF (.NOT. FINSH__CAT1(CIELM) ) THEN
               CALL CAT1_FINCR (CI, STATUS)
               FINSH__CAT1(CIELM) = .TRUE.
            END IF

*
*          Write the value in an appropriate fashion, depending on
*          whether the part is a column, vector column element or
*          parameter.

            IF (IDTYPE .EQ. CAT__FITYP) THEN

*
*             The part is a column.

               CURROW = CROW__CAT1(CIELM)

               CALL CAT1_FIOR (.FALSE., PI, 1, CURROW, VALUE, NULFLG,
     :           STATUS)

            ELSE IF (IDTYPE .EQ. CAT__FETYP) THEN

*
*             The part is a vector column element.

               CURROW = CROW__CAT1(CIELM)

               CALL CAT_TIQAI (PI, 'BASEID', PIB, STATUS)
               CALL CAT_TIQAI (PI, 'ELEM', ELEM, STATUS)

               CALL CAT1_FIOR (.FALSE., PIB, ELEM, CURROW, VALUE,
     :           NULFLG, STATUS)

            ELSE IF (IDTYPE .EQ. CAT__QITYP) THEN

*
*             The part is a parameter.

               CALL CAT_TATTR (PI, 'VALUE', VALUE, STATUS)

            ELSE
               STATUS = CAT__INVPT

            END IF

         ELSE
            STATUS = CAT__INVWT

         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            ERRMSG = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('CAT_PUT0R: error putting value '/
     :        /'for component ', ERRMSG, ERRLEN)

            ESTAT = CAT__OK
            CALL CAT_TIQAC (PI, 'NAME', CMPNAM, ESTAT)

            IF (CMPNAM .NE. ' ') THEN
               LCMPNM = CHR_LEN(CMPNAM)
               CALL CHR_PUTC (CMPNAM(1 : LCMPNM), ERRMSG, ERRLEN)
            ELSE
               CALL CHR_PUTC ('<Unknown>', ERRMSG, ERRLEN)
            END IF

            IF (IDTYPE .EQ. CAT__FITYP  .OR.  IDTYPE .EQ. CAT__FETYP)
     :        THEN
               CALL CHR_PUTC (', row ', ERRMSG, ERRLEN)
               CALL CHR_PUTI (CURROW, ERRMSG, ERRLEN)
            END IF

            CALL CHR_PUTC ('.', ERRMSG, ERRLEN)

            CALL CAT1_ERREP ('CAT_PUT0R_ERR', ERRMSG(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT_PUT0W (PI, VALUE, NULFLG, STATUS)
*+
*  Name:
*     CAT_PUT0W
*  Purpose:
*     Put a value to a scalar part (field or column).
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_PUT0W (PI, VALUE, NULFLG; STATUS)
*  Description:
*     Put a value to a scalar part (field or column). If the identifier
*     refers to a field the value is written to the current row buffer.
*  Arguments:
*     PI  =  INTEGER (Given)
*        Part identifier.
*     VALUE  =  INTEGER*2 (Given)
*        Value to PUT to the part.
*     NULFLG  =  LOGICAL (Given)
*        A flag indicating whether or not the value is null or not:
*        .TRUE.  - The value is null,
*        .FALSE. - The value is not null.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Obtain the identifier for the parent catalogue.
*     Obtain the access mode of the catalogue.
*     If the catalogue has been opened for 'WRITE' access then
*       If the initial creation of the columns and parameters is
*       not finished then
*         Finish the initial creation of the columns and parameters.
*       end if
*       Determine whether the part is a column or parameter.
*       If the part is a column then
*         Write the value to the column's field in the current row.
*       else the part is a vector column element then
*         Determine the column to which the part refers.
*         Determine the element number to which the part refers.
*         Write the value to the appropriate vector column element in
*         the current row.
*       else the part is a parameter
*         Write the value to the parameter.
*       else
*         Set the status.
*       end if
*     else
*       Set the status.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     3/5/93   (ACD): Prologue only.
*     13/8/93  (ACD): Original version.
*     11/10/93 (ACD): First stable version.
*     24/1/94  (ACD): Modified error reporting.
*     2/2/94   (ACD): Added handling vector column element.
*     10/6/98  (ACD): Improved the error reporting.
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
      INCLUDE 'CAT1_CATS_CMN'     ! Catalogues common block.
*  Arguments Given:
      INTEGER
     :  PI
      INTEGER*2
     :  VALUE
      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  CI,      ! Identifier to the part's parent catalogue.
     :  CIELM,   ! Common block array element for the parent catalogue.
     :  MODE,    ! Access mode of the parent catalogue.
     :  IDTYPE,  ! Type of the part (column, parameter etc).
     :  CURROW,  ! Current row number.
     :  PIB,     ! Base identifier for vector column.
     :  ELEM,    ! Element in vector column.
     :  ERRLEN,  ! Length of ERRMSG (excl. trail. blanks).
     :  LCMPNM,  !   "    "  CMPNAM ( "  .   "  .   "   ).
     :  ESTAT    ! Status generating error message.
      CHARACTER
     :  ERRMSG*75,           ! Error message.
     :  CMPNAM*(CAT__SZCMP)  ! Component name.
*.

      IF (STATUS .EQ. CAT__OK) THEN
         CURROW = 0
         IDTYPE = 0

*
*       Obtain the identifier for the part's parent catalogue and the
*       corresponding common block array element.

         CALL CAT_TIDPR (PI, CI, STATUS)
         CALL CAT1_CIELM (CI, CIELM, STATUS)

*
*       Obtain the access mode for the catalogue.

         MODE = MODE__CAT1(CIELM)

*
*       Proceed if the catalogue is opened for write.

         IF (MODE .EQ. CAT1__MDWRT) THEN

*
*          Determine whether the part is a column or parameter.

            CALL CAT_TIDTP (PI, IDTYPE, STATUS)

*
*          If the initial creation of the columns and parameters is
*          not finished then finish the creation.

            IF (.NOT. FINSH__CAT1(CIELM) ) THEN
               CALL CAT1_FINCR (CI, STATUS)
               FINSH__CAT1(CIELM) = .TRUE.
            END IF

*
*          Write the value in an appropriate fashion, depending on
*          whether the part is a column, vector column element or
*          parameter.

            IF (IDTYPE .EQ. CAT__FITYP) THEN

*
*             The part is a column.

               CURROW = CROW__CAT1(CIELM)

               CALL CAT1_FIOW (.FALSE., PI, 1, CURROW, VALUE, NULFLG,
     :           STATUS)

            ELSE IF (IDTYPE .EQ. CAT__FETYP) THEN

*
*             The part is a vector column element.

               CURROW = CROW__CAT1(CIELM)

               CALL CAT_TIQAI (PI, 'BASEID', PIB, STATUS)
               CALL CAT_TIQAI (PI, 'ELEM', ELEM, STATUS)

               CALL CAT1_FIOW (.FALSE., PIB, ELEM, CURROW, VALUE,
     :           NULFLG, STATUS)

            ELSE IF (IDTYPE .EQ. CAT__QITYP) THEN

*
*             The part is a parameter.

               CALL CAT_TATTW (PI, 'VALUE', VALUE, STATUS)

            ELSE
               STATUS = CAT__INVPT

            END IF

         ELSE
            STATUS = CAT__INVWT

         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            ERRMSG = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('CAT_PUT0W: error putting value '/
     :        /'for component ', ERRMSG, ERRLEN)

            ESTAT = CAT__OK
            CALL CAT_TIQAC (PI, 'NAME', CMPNAM, ESTAT)

            IF (CMPNAM .NE. ' ') THEN
               LCMPNM = CHR_LEN(CMPNAM)
               CALL CHR_PUTC (CMPNAM(1 : LCMPNM), ERRMSG, ERRLEN)
            ELSE
               CALL CHR_PUTC ('<Unknown>', ERRMSG, ERRLEN)
            END IF

            IF (IDTYPE .EQ. CAT__FITYP  .OR.  IDTYPE .EQ. CAT__FETYP)
     :        THEN
               CALL CHR_PUTC (', row ', ERRMSG, ERRLEN)
               CALL CHR_PUTI (CURROW, ERRMSG, ERRLEN)
            END IF

            CALL CHR_PUTC ('.', ERRMSG, ERRLEN)

            CALL CAT1_ERREP ('CAT_PUT0W_ERR', ERRMSG(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
