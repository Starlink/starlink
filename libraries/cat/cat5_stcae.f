      SUBROUTINE CAT5_STCAE (ROWS, CSIZE, ROW, VALUE, COLIST, STATUS)
*+
*  Name:
*     CAT5_STAE<t>
*  Purpose:
*     Set a specified element in a character array of column values.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT5_STCAE (ROWS, CSIZE, ROW, VALUE; COLIST; STATUS)
*  Description:
*     Set a specified element in a character array of column values.
*  Arguments:
*     ROWS  =  INTEGER (Given)
*        Number of rows in the column.
*     CSIZE  -  INTEGER (Given)
*        Size of each element (corresponding to a field) in the
*        character array.
*     ROW  =  INTEGER (Given)
*        Row number to be set.
*     VALUE  =  CHARACTER*(*) (Given)
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
      CHARACTER
     :  VALUE*(*)
*  Arguments Given and Returned:
      INTEGER
     :  COLIST(CSIZE, ROWS)
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      CHARACTER
     :  ERRMSG*75   ! Text of error message.
      INTEGER
     :  LVALUE,     ! Length of value (excl. trail. blanks).
     :  LAST,       ! Last element to be set.
     :  LOOP,       ! Loop index.
     :  ERRLEN      ! Length of ERRMSG (excl. trail. blanks).
*.

      IF (STATUS .EQ. CAT__OK) THEN

         IF (ROW .GT. 0  .AND.  ROW .LE. ROWS) THEN
            IF (VALUE .NE. ' ') THEN
               LVALUE = CHR_LEN(VALUE)

               LAST = MIN(LVALUE, CSIZE)

               DO LOOP = 1, LAST
                  COLIST(LOOP, ROW) = ICHAR(VALUE(LOOP : LOOP) )
               END DO

               DO LOOP = LVALUE+1, CSIZE
                  COLIST(LOOP, ROW) = ICHAR(' ')
               END DO

            ELSE
               DO LOOP = 1, CSIZE
                  COLIST(LOOP, ROW) = ICHAR(' ')
               END DO

            END IF

         ELSE
            STATUS = CAT__INVRW

            ERRMSG = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('Error: row number ', ERRMSG, ERRLEN)
            CALL CHR_PUTI (ROW, ERRMSG, ERRLEN)
            CALL CHR_PUTC (' is out of range.', ERRMSG, ERRLEN)

            CALL CAT1_ERREP ('CAT5_STCAE_ERR', ERRMSG(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
