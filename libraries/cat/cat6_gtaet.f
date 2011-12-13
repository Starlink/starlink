      SUBROUTINE CAT6_GTAEB (ROWS, ROW, COLIST, VALUE, STATUS)
*+
*  Name:
*     CAT6_GTAEB
*  Purpose:
*     Get a specified element in an array of column values.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT6_GTAEB (ROWS, ROW, COLIST; VALUE; STATUS)
*  Description:
*     Get a specified element in an array of column values.
*  Arguments:
*     ROWS  =  INTEGER (Given)
*        Number of rows in the column.
*     ROW  =  INTEGER (Given)
*        Row number to be set.
*     COLIST(ROWS)  =  BYTE (Given)
*        Array of column values.
*     VALUE  =  BYTE (Returned)
*        Field value to be got (that is, the value of the column for
*        the given row).
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the row number is inside the permitted range then
*       Get the value.
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
*     15/6/99 (ACD): Original version (from CAT5_GTAET.GEN).
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
     :  COLIST(ROWS)
*  Arguments Returned:
      BYTE
     :  VALUE
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
            VALUE = COLIST(ROW)

         ELSE
            STATUS = CAT__INVRW

            ERRMSG = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('Error: row number ', ERRMSG, ERRLEN)
            CALL CHR_PUTI (ROW, ERRMSG, ERRLEN)
            CALL CHR_PUTC (' is out of range.', ERRMSG, ERRLEN)

            CALL CAT1_ERREP ('CAT6_GTAEB_ERR', ERRMSG(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT6_GTAEC (ROWS, ROW, COLIST, VALUE, STATUS)
*+
*  Name:
*     CAT6_GTAEC
*  Purpose:
*     Get a specified element in an array of column values.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT6_GTAEC (ROWS, ROW, COLIST; VALUE; STATUS)
*  Description:
*     Get a specified element in an array of column values.
*  Arguments:
*     ROWS  =  INTEGER (Given)
*        Number of rows in the column.
*     ROW  =  INTEGER (Given)
*        Row number to be set.
*     COLIST(ROWS)  =  CHARACTER*(*) (Given)
*        Array of column values.
*     VALUE  =  CHARACTER*(*) (Returned)
*        Field value to be got (that is, the value of the column for
*        the given row).
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the row number is inside the permitted range then
*       Get the value.
*     else
*       Set the status.
*       Report an error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     15/6/99 (ACD): Original version (from CAT5_GTAET.GEN).
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
     :  COLIST(ROWS)
*  Arguments Returned:
      CHARACTER*(*)
     :  VALUE
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
            VALUE = COLIST(ROW)

         ELSE
            STATUS = CAT__INVRW

            ERRMSG = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('Error: row number ', ERRMSG, ERRLEN)
            CALL CHR_PUTI (ROW, ERRMSG, ERRLEN)
            CALL CHR_PUTC (' is out of range.', ERRMSG, ERRLEN)

            CALL CAT1_ERREP ('CAT6_GTAEC_ERR', ERRMSG(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT6_GTAED (ROWS, ROW, COLIST, VALUE, STATUS)
*+
*  Name:
*     CAT6_GTAED
*  Purpose:
*     Get a specified element in an array of column values.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT6_GTAED (ROWS, ROW, COLIST; VALUE; STATUS)
*  Description:
*     Get a specified element in an array of column values.
*  Arguments:
*     ROWS  =  INTEGER (Given)
*        Number of rows in the column.
*     ROW  =  INTEGER (Given)
*        Row number to be set.
*     COLIST(ROWS)  =  DOUBLE PRECISION (Given)
*        Array of column values.
*     VALUE  =  DOUBLE PRECISION (Returned)
*        Field value to be got (that is, the value of the column for
*        the given row).
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the row number is inside the permitted range then
*       Get the value.
*     else
*       Set the status.
*       Report an error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     15/6/99 (ACD): Original version (from CAT5_GTAET.GEN).
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
     :  COLIST(ROWS)
*  Arguments Returned:
      DOUBLE PRECISION
     :  VALUE
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
            VALUE = COLIST(ROW)

         ELSE
            STATUS = CAT__INVRW

            ERRMSG = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('Error: row number ', ERRMSG, ERRLEN)
            CALL CHR_PUTI (ROW, ERRMSG, ERRLEN)
            CALL CHR_PUTC (' is out of range.', ERRMSG, ERRLEN)

            CALL CAT1_ERREP ('CAT6_GTAED_ERR', ERRMSG(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT6_GTAEI (ROWS, ROW, COLIST, VALUE, STATUS)
*+
*  Name:
*     CAT6_GTAEI
*  Purpose:
*     Get a specified element in an array of column values.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT6_GTAEI (ROWS, ROW, COLIST; VALUE; STATUS)
*  Description:
*     Get a specified element in an array of column values.
*  Arguments:
*     ROWS  =  INTEGER (Given)
*        Number of rows in the column.
*     ROW  =  INTEGER (Given)
*        Row number to be set.
*     COLIST(ROWS)  =  INTEGER (Given)
*        Array of column values.
*     VALUE  =  INTEGER (Returned)
*        Field value to be got (that is, the value of the column for
*        the given row).
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the row number is inside the permitted range then
*       Get the value.
*     else
*       Set the status.
*       Report an error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     15/6/99 (ACD): Original version (from CAT5_GTAET.GEN).
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
     :  COLIST(ROWS)
*  Arguments Returned:
      INTEGER
     :  VALUE
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
            VALUE = COLIST(ROW)

         ELSE
            STATUS = CAT__INVRW

            ERRMSG = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('Error: row number ', ERRMSG, ERRLEN)
            CALL CHR_PUTI (ROW, ERRMSG, ERRLEN)
            CALL CHR_PUTC (' is out of range.', ERRMSG, ERRLEN)

            CALL CAT1_ERREP ('CAT6_GTAEI_ERR', ERRMSG(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT6_GTAEL (ROWS, ROW, COLIST, VALUE, STATUS)
*+
*  Name:
*     CAT6_GTAEL
*  Purpose:
*     Get a specified element in an array of column values.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT6_GTAEL (ROWS, ROW, COLIST; VALUE; STATUS)
*  Description:
*     Get a specified element in an array of column values.
*  Arguments:
*     ROWS  =  INTEGER (Given)
*        Number of rows in the column.
*     ROW  =  INTEGER (Given)
*        Row number to be set.
*     COLIST(ROWS)  =  LOGICAL (Given)
*        Array of column values.
*     VALUE  =  LOGICAL (Returned)
*        Field value to be got (that is, the value of the column for
*        the given row).
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the row number is inside the permitted range then
*       Get the value.
*     else
*       Set the status.
*       Report an error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     15/6/99 (ACD): Original version (from CAT5_GTAET.GEN).
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
     :  COLIST(ROWS)
*  Arguments Returned:
      LOGICAL
     :  VALUE
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
            VALUE = COLIST(ROW)

         ELSE
            STATUS = CAT__INVRW

            ERRMSG = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('Error: row number ', ERRMSG, ERRLEN)
            CALL CHR_PUTI (ROW, ERRMSG, ERRLEN)
            CALL CHR_PUTC (' is out of range.', ERRMSG, ERRLEN)

            CALL CAT1_ERREP ('CAT6_GTAEL_ERR', ERRMSG(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT6_GTAER (ROWS, ROW, COLIST, VALUE, STATUS)
*+
*  Name:
*     CAT6_GTAER
*  Purpose:
*     Get a specified element in an array of column values.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT6_GTAER (ROWS, ROW, COLIST; VALUE; STATUS)
*  Description:
*     Get a specified element in an array of column values.
*  Arguments:
*     ROWS  =  INTEGER (Given)
*        Number of rows in the column.
*     ROW  =  INTEGER (Given)
*        Row number to be set.
*     COLIST(ROWS)  =  REAL (Given)
*        Array of column values.
*     VALUE  =  REAL (Returned)
*        Field value to be got (that is, the value of the column for
*        the given row).
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the row number is inside the permitted range then
*       Get the value.
*     else
*       Set the status.
*       Report an error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     15/6/99 (ACD): Original version (from CAT5_GTAET.GEN).
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
     :  COLIST(ROWS)
*  Arguments Returned:
      REAL
     :  VALUE
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
            VALUE = COLIST(ROW)

         ELSE
            STATUS = CAT__INVRW

            ERRMSG = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('Error: row number ', ERRMSG, ERRLEN)
            CALL CHR_PUTI (ROW, ERRMSG, ERRLEN)
            CALL CHR_PUTC (' is out of range.', ERRMSG, ERRLEN)

            CALL CAT1_ERREP ('CAT6_GTAER_ERR', ERRMSG(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT6_GTAEW (ROWS, ROW, COLIST, VALUE, STATUS)
*+
*  Name:
*     CAT6_GTAEW
*  Purpose:
*     Get a specified element in an array of column values.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT6_GTAEW (ROWS, ROW, COLIST; VALUE; STATUS)
*  Description:
*     Get a specified element in an array of column values.
*  Arguments:
*     ROWS  =  INTEGER (Given)
*        Number of rows in the column.
*     ROW  =  INTEGER (Given)
*        Row number to be set.
*     COLIST(ROWS)  =  INTEGER*2 (Given)
*        Array of column values.
*     VALUE  =  INTEGER*2 (Returned)
*        Field value to be got (that is, the value of the column for
*        the given row).
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the row number is inside the permitted range then
*       Get the value.
*     else
*       Set the status.
*       Report an error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     15/6/99 (ACD): Original version (from CAT5_GTAET.GEN).
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
     :  COLIST(ROWS)
*  Arguments Returned:
      INTEGER*2
     :  VALUE
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
            VALUE = COLIST(ROW)

         ELSE
            STATUS = CAT__INVRW

            ERRMSG = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('Error: row number ', ERRMSG, ERRLEN)
            CALL CHR_PUTI (ROW, ERRMSG, ERRLEN)
            CALL CHR_PUTC (' is out of range.', ERRMSG, ERRLEN)

            CALL CAT1_ERREP ('CAT6_GTAEW_ERR', ERRMSG(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
