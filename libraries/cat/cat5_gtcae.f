      SUBROUTINE CAT5_GTCAE (ROWS, CSIZE, ROW, COLIST, VALUE, STATUS)
*+
*  Name:
*     CAT5_GTCAE
*  Purpose:
*     Get a specified element in a character array of column values.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT5_GTCAE (ROWS, CSIZE, ROW, COLIST; VALUE; STATUS)
*  Description:
*     Get a specified element in a character array of column values.
*
*     Note that character arrays are stored as coded integers.
*  Arguments:
*     ROWS  =  INTEGER (Given)
*        Number of rows in the column.
*     CSIZE  -  INTEGER (Given)
*        Size of each element (corresponding to a field) in the
*        character array.
*     ROW  =  INTEGER (Given)
*        Row number to be set.
*     COLIST(CSIZE, ROWS)  =  INTEGER (Given)
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
*     16/7/96 (ACD): Original version.
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
     :  CSIZE,
     :  ROW
      INTEGER
     :  COLIST(CSIZE, ROWS)
*  Arguments Returned:
      CHARACTER
     :  VALUE*(*)
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      CHARACTER
     :  ERRMSG*75   ! Text of error message.
      INTEGER
     :  LVALUE,     ! Declared length of value.
     :  LAST,       ! Last element to be copied.
     :  LOOP,       ! Loop index.
     :  ERRLEN      ! Length of ERRMSG (excl. trail. blanks).
*.

      IF (STATUS .EQ. CAT__OK) THEN

         IF (ROW .GT. 0  .AND.  ROW .LE. ROWS) THEN
            VALUE = ' '

            LVALUE = LEN(VALUE)
            LAST = MIN(CSIZE, LVALUE)

            DO LOOP = 1, LAST
               VALUE(LOOP : LOOP) = CHAR(COLIST(LOOP, ROW) )
            END DO

         ELSE
            STATUS = CAT__INVRW

            ERRMSG = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('Error: row number ', ERRMSG, ERRLEN)
            CALL CHR_PUTI (ROW, ERRMSG, ERRLEN)
            CALL CHR_PUTC (' is out of range.', ERRMSG, ERRLEN)

            CALL CAT1_ERREP ('CAT5_GTCAE_ERR', ERRMSG(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
