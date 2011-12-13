      SUBROUTINE CAT1_GNSRT (CI, FI, ROWS, ORDER, NULIST, COLVAL,
     :  COLNUM, SEQNUM, SORLST, STATUS)
*+
*  Name:
*     CAT1_GNSRT
*  Purpose:
*     Generate an array of row numbers sorted on a column,
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_GNSRT (CI, FI, ROWS, ORDER; NULIST, COLVAL, COLNUM,
*        SEQNUM; SORLST; STATUS)
*  Description:
*     Generate an array of numbers sorted to correspond to ascending
*     order for some column.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     FI  =  INTEGER (Given)
*        Column identifier.
*     ROWS  =  INTEGER (Given)
*        Number of rows in the catalogue.
*     ORDER  =  INTEGER (Given)
*        Order of the numbers in the return array, coded as follows:
*        CAT__ASCND  -  ascending,
*        CAT__DSCND  -  descending.
*     NULIST(ROWS)  =  INTEGER (Work space)
*        Work array to hold the list of row numbers for which the
*        field of the specified column is null.
*     COLVAL(ROWS)  =  DOUBLE PRECISION (Work space)
*        Work array to hold the valid fields read from the specified
*        column.
*     COLNUM(ROWS)  =  INTEGER (Work space)
*        Work array to hold the row numbers corresponding to each valid
*        field in the specified column.
*     SEQNUM(ROWS)  =  INTEGER (Work space)
*        Work array to hold the sequence numbers for the rows to be
*        sorted.
*     SORLST(ROWS)  =  INTEGER (Returned)
*        Array holding a set of row numbers for the catalogue, sorted
*        to correspond to ascending order for the specified column.
*        Any rows for which the field of the column is null are appended
*        at the bottom of the list.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Initialise the counts of valid and null rows.
*     For every row in the catalogue.
*       Read the next row.
*       Get the value for the specified column.
*       If the value is not null then
*         Append details of the field to list of valid values.
*       else
*         Append the row number to the list of null rows.
*       end if
*     end for
*     If there were any non-null rows then
*       Sort the list of values (and associated row numbers) into
*       ascending or descending order.
*     end if
*     If there were any null values then
*       Append the list of null rows to the end of the sorted list.
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
*     24/11/94 (ACD): Original version.
*     20/4/95  (ACD): First stable version.
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
     :  FI,
     :  ROWS,
     :  ORDER
*  Arguments Given and Returned:
      INTEGER
     :  NULIST(ROWS),
     :  COLNUM(ROWS),
     :  SEQNUM(ROWS)
      DOUBLE PRECISION
     :  COLVAL(ROWS)
*  Arguments Returned:
      INTEGER
     :  SORLST(ROWS)
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      INTEGER
     :  NUMVAL,   ! Number of valid rows.
     :  NUMNUL,   ! Number of null rows.
     :  CURROW    ! Current row number.
      DOUBLE PRECISION
     :  FLDVAL    ! Value of field for current row.
      LOGICAL
     :  NULFLG    ! Null value flag for the current row.
*.

      IF (STATUS .EQ. CAT__OK) THEN

C        print1999, ci, fi, rows
C1999    format(1x, 'ci, fi, rows: ', i10, i10, i10)

*
*       Initialise the counts of valid and null rows.

         NUMVAL = 0
         NUMNUL = 0

*
*       Read the field for the chosen column for every row in the
*       catalogue, and append it to either the list of valid rows
*       or the list of null rows, as appropriate.

         DO CURROW = 1, ROWS
            CALL CAT_RGET (CI, CURROW, STATUS)

            CALL CAT_EGT0D (FI, FLDVAL, NULFLG, STATUS)

            IF (.NOT. NULFLG) THEN
               NUMVAL = NUMVAL + 1

               COLVAL(NUMVAL) = FLDVAL
               COLNUM(NUMVAL) = CURROW
            ELSE
               NUMNUL = NUMNUL + 1

               NULIST(NUMNUL) = CURROW
            END IF
         END DO

*
*       If there are any non-null rows then sort them.

         IF (NUMVAL .GT. 0) THEN

            DO CURROW = 1, NUMVAL
               SEQNUM(CURROW) = CURROW
            END DO

            CALL CAT1_QSRTD (NUMVAL, COLVAL, SEQNUM, STATUS)

            IF (ORDER .EQ. CAT__DSCND) THEN
               DO CURROW = 1, NUMVAL
                  SORLST(NUMVAL+1-CURROW) = COLNUM(SEQNUM(CURROW))
               END DO
            ELSE
               DO CURROW = 1, NUMVAL
                  SORLST(CURROW) = COLNUM(SEQNUM(CURROW))
               END DO
            END IF
         END IF

*
*       If there are any sorted rows then append them to the sorted
*       list.

         IF (NUMNUL .GT. 0) THEN
            DO CURROW = 1, NUMNUL
               SORLST(NUMVAL + CURROW) = NULIST(CURROW)
            END DO
         END IF

C        do currow = 1, rows
C           print2000, currow, sorlst(currow)
C2000       format(1x, 'currow, sorlst(currow): ', i10, i10)
C        end do

      END IF

      END
