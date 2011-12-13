      SUBROUTINE CAT5_STAEB (ROWS, ROW, VALUE, COLIST, STATUS)
*+
*  Name:
*     CAT5_STAEB
*  Purpose:
*     Set a specified element in an array of column values.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT5_STAEB (ROWS, ROW, VALUE; COLIST; STATUS)
*  Description:
*     Set a specified element in an array of column values.
*  Arguments:
*     ROWS  =  INTEGER (Given)
*        Number of rows in the column.
*     ROW  =  INTEGER (Given)
*        Row number to be set.
*     VALUE  =  BYTE (Given)
*        Field value to be set (that is, the value of the column for
*        the given row).
*     COLIST(ROWS)  =  BYTE (Given and Returned)
*        Array of column values.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the row number is inside the permitted range then
*       Set the value.
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
*     15/7/96 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  ROWS,
     :  ROW
      BYTE
     :  VALUE
*  Arguments Given and Returned:
      BYTE
     :  COLIST(ROWS)
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      CHARACTER
     :  ERRMSG*75   ! Text of error message.
      INTEGER
     :  ERRLEN      ! Length of ERRMSG (excl. trail. blanks).
*.

      IF (STATUS .EQ. CAT__OK) THEN

         IF (ROW .GT. 0  .AND.  ROW .LE. ROWS) THEN
            COLIST(ROW) = VALUE

         ELSE
            STATUS = CAT__INVRW

            ERRMSG = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('Error: row number ', ERRMSG, ERRLEN)
            CALL CHR_PUTI (ROW, ERRMSG, ERRLEN)
            CALL CHR_PUTC (' is out of range.', ERRMSG, ERRLEN)

            CALL CAT1_ERREP ('CAT5_STAEB_ERR', ERRMSG(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT5_STAEC (ROWS, ROW, VALUE, COLIST, STATUS)
*+
*  Name:
*     CAT5_STAEC
*  Purpose:
*     Set a specified element in an array of column values.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT5_STAEC (ROWS, ROW, VALUE; COLIST; STATUS)
*  Description:
*     Set a specified element in an array of column values.
*  Arguments:
*     ROWS  =  INTEGER (Given)
*        Number of rows in the column.
*     ROW  =  INTEGER (Given)
*        Row number to be set.
*     VALUE  =  CHARACTER*(*) (Given)
*        Field value to be set (that is, the value of the column for
*        the given row).
*     COLIST(ROWS)  =  CHARACTER*(*) (Given and Returned)
*        Array of column values.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the row number is inside the permitted range then
*       Set the value.
*     else
*       Set the status.
*       Report an error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     15/7/96 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  ROWS,
     :  ROW
      CHARACTER*(*)
     :  VALUE
*  Arguments Given and Returned:
      CHARACTER*(*)
     :  COLIST(ROWS)
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      CHARACTER
     :  ERRMSG*75   ! Text of error message.
      INTEGER
     :  ERRLEN      ! Length of ERRMSG (excl. trail. blanks).
*.

      IF (STATUS .EQ. CAT__OK) THEN

         IF (ROW .GT. 0  .AND.  ROW .LE. ROWS) THEN
            COLIST(ROW) = VALUE

         ELSE
            STATUS = CAT__INVRW

            ERRMSG = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('Error: row number ', ERRMSG, ERRLEN)
            CALL CHR_PUTI (ROW, ERRMSG, ERRLEN)
            CALL CHR_PUTC (' is out of range.', ERRMSG, ERRLEN)

            CALL CAT1_ERREP ('CAT5_STAEC_ERR', ERRMSG(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT5_STAED (ROWS, ROW, VALUE, COLIST, STATUS)
*+
*  Name:
*     CAT5_STAED
*  Purpose:
*     Set a specified element in an array of column values.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT5_STAED (ROWS, ROW, VALUE; COLIST; STATUS)
*  Description:
*     Set a specified element in an array of column values.
*  Arguments:
*     ROWS  =  INTEGER (Given)
*        Number of rows in the column.
*     ROW  =  INTEGER (Given)
*        Row number to be set.
*     VALUE  =  DOUBLE PRECISION (Given)
*        Field value to be set (that is, the value of the column for
*        the given row).
*     COLIST(ROWS)  =  DOUBLE PRECISION (Given and Returned)
*        Array of column values.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the row number is inside the permitted range then
*       Set the value.
*     else
*       Set the status.
*       Report an error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     15/7/96 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  ROWS,
     :  ROW
      DOUBLE PRECISION
     :  VALUE
*  Arguments Given and Returned:
      DOUBLE PRECISION
     :  COLIST(ROWS)
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      CHARACTER
     :  ERRMSG*75   ! Text of error message.
      INTEGER
     :  ERRLEN      ! Length of ERRMSG (excl. trail. blanks).
*.

      IF (STATUS .EQ. CAT__OK) THEN

         IF (ROW .GT. 0  .AND.  ROW .LE. ROWS) THEN
            COLIST(ROW) = VALUE

         ELSE
            STATUS = CAT__INVRW

            ERRMSG = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('Error: row number ', ERRMSG, ERRLEN)
            CALL CHR_PUTI (ROW, ERRMSG, ERRLEN)
            CALL CHR_PUTC (' is out of range.', ERRMSG, ERRLEN)

            CALL CAT1_ERREP ('CAT5_STAED_ERR', ERRMSG(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT5_STAEI (ROWS, ROW, VALUE, COLIST, STATUS)
*+
*  Name:
*     CAT5_STAEI
*  Purpose:
*     Set a specified element in an array of column values.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT5_STAEI (ROWS, ROW, VALUE; COLIST; STATUS)
*  Description:
*     Set a specified element in an array of column values.
*  Arguments:
*     ROWS  =  INTEGER (Given)
*        Number of rows in the column.
*     ROW  =  INTEGER (Given)
*        Row number to be set.
*     VALUE  =  INTEGER (Given)
*        Field value to be set (that is, the value of the column for
*        the given row).
*     COLIST(ROWS)  =  INTEGER (Given and Returned)
*        Array of column values.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the row number is inside the permitted range then
*       Set the value.
*     else
*       Set the status.
*       Report an error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     15/7/96 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  ROWS,
     :  ROW
      INTEGER
     :  VALUE
*  Arguments Given and Returned:
      INTEGER
     :  COLIST(ROWS)
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      CHARACTER
     :  ERRMSG*75   ! Text of error message.
      INTEGER
     :  ERRLEN      ! Length of ERRMSG (excl. trail. blanks).
*.

      IF (STATUS .EQ. CAT__OK) THEN

         IF (ROW .GT. 0  .AND.  ROW .LE. ROWS) THEN
            COLIST(ROW) = VALUE

         ELSE
            STATUS = CAT__INVRW

            ERRMSG = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('Error: row number ', ERRMSG, ERRLEN)
            CALL CHR_PUTI (ROW, ERRMSG, ERRLEN)
            CALL CHR_PUTC (' is out of range.', ERRMSG, ERRLEN)

            CALL CAT1_ERREP ('CAT5_STAEI_ERR', ERRMSG(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT5_STAEL (ROWS, ROW, VALUE, COLIST, STATUS)
*+
*  Name:
*     CAT5_STAEL
*  Purpose:
*     Set a specified element in an array of column values.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT5_STAEL (ROWS, ROW, VALUE; COLIST; STATUS)
*  Description:
*     Set a specified element in an array of column values.
*  Arguments:
*     ROWS  =  INTEGER (Given)
*        Number of rows in the column.
*     ROW  =  INTEGER (Given)
*        Row number to be set.
*     VALUE  =  LOGICAL (Given)
*        Field value to be set (that is, the value of the column for
*        the given row).
*     COLIST(ROWS)  =  LOGICAL (Given and Returned)
*        Array of column values.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the row number is inside the permitted range then
*       Set the value.
*     else
*       Set the status.
*       Report an error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     15/7/96 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  ROWS,
     :  ROW
      LOGICAL
     :  VALUE
*  Arguments Given and Returned:
      LOGICAL
     :  COLIST(ROWS)
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      CHARACTER
     :  ERRMSG*75   ! Text of error message.
      INTEGER
     :  ERRLEN      ! Length of ERRMSG (excl. trail. blanks).
*.

      IF (STATUS .EQ. CAT__OK) THEN

         IF (ROW .GT. 0  .AND.  ROW .LE. ROWS) THEN
            COLIST(ROW) = VALUE

         ELSE
            STATUS = CAT__INVRW

            ERRMSG = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('Error: row number ', ERRMSG, ERRLEN)
            CALL CHR_PUTI (ROW, ERRMSG, ERRLEN)
            CALL CHR_PUTC (' is out of range.', ERRMSG, ERRLEN)

            CALL CAT1_ERREP ('CAT5_STAEL_ERR', ERRMSG(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT5_STAER (ROWS, ROW, VALUE, COLIST, STATUS)
*+
*  Name:
*     CAT5_STAER
*  Purpose:
*     Set a specified element in an array of column values.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT5_STAER (ROWS, ROW, VALUE; COLIST; STATUS)
*  Description:
*     Set a specified element in an array of column values.
*  Arguments:
*     ROWS  =  INTEGER (Given)
*        Number of rows in the column.
*     ROW  =  INTEGER (Given)
*        Row number to be set.
*     VALUE  =  REAL (Given)
*        Field value to be set (that is, the value of the column for
*        the given row).
*     COLIST(ROWS)  =  REAL (Given and Returned)
*        Array of column values.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the row number is inside the permitted range then
*       Set the value.
*     else
*       Set the status.
*       Report an error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     15/7/96 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  ROWS,
     :  ROW
      REAL
     :  VALUE
*  Arguments Given and Returned:
      REAL
     :  COLIST(ROWS)
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      CHARACTER
     :  ERRMSG*75   ! Text of error message.
      INTEGER
     :  ERRLEN      ! Length of ERRMSG (excl. trail. blanks).
*.

      IF (STATUS .EQ. CAT__OK) THEN

         IF (ROW .GT. 0  .AND.  ROW .LE. ROWS) THEN
            COLIST(ROW) = VALUE

         ELSE
            STATUS = CAT__INVRW

            ERRMSG = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('Error: row number ', ERRMSG, ERRLEN)
            CALL CHR_PUTI (ROW, ERRMSG, ERRLEN)
            CALL CHR_PUTC (' is out of range.', ERRMSG, ERRLEN)

            CALL CAT1_ERREP ('CAT5_STAER_ERR', ERRMSG(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT5_STAEW (ROWS, ROW, VALUE, COLIST, STATUS)
*+
*  Name:
*     CAT5_STAEW
*  Purpose:
*     Set a specified element in an array of column values.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT5_STAEW (ROWS, ROW, VALUE; COLIST; STATUS)
*  Description:
*     Set a specified element in an array of column values.
*  Arguments:
*     ROWS  =  INTEGER (Given)
*        Number of rows in the column.
*     ROW  =  INTEGER (Given)
*        Row number to be set.
*     VALUE  =  INTEGER*2 (Given)
*        Field value to be set (that is, the value of the column for
*        the given row).
*     COLIST(ROWS)  =  INTEGER*2 (Given and Returned)
*        Array of column values.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the row number is inside the permitted range then
*       Set the value.
*     else
*       Set the status.
*       Report an error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     15/7/96 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  ROWS,
     :  ROW
      INTEGER*2
     :  VALUE
*  Arguments Given and Returned:
      INTEGER*2
     :  COLIST(ROWS)
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      CHARACTER
     :  ERRMSG*75   ! Text of error message.
      INTEGER
     :  ERRLEN      ! Length of ERRMSG (excl. trail. blanks).
*.

      IF (STATUS .EQ. CAT__OK) THEN

         IF (ROW .GT. 0  .AND.  ROW .LE. ROWS) THEN
            COLIST(ROW) = VALUE

         ELSE
            STATUS = CAT__INVRW

            ERRMSG = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('Error: row number ', ERRMSG, ERRLEN)
            CALL CHR_PUTI (ROW, ERRMSG, ERRLEN)
            CALL CHR_PUTC (' is out of range.', ERRMSG, ERRLEN)

            CALL CAT1_ERREP ('CAT5_STAEW_ERR', ERRMSG(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
