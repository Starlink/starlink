      SUBROUTINE CAT1_SELCT (EI, REJFLG, CI, ROWS, NUMSEL, SELIST,
     :  NUMREJ, REJIST, STATUS)
*+
*  Name:
*     CAT1_SELCT
*  Purpose:
*     Select rows from a catalogue according to some criterion.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_SELCT (EI, REJFLG, CI, ROWS; NUMSEL, SELIST, NUMREJ,
*       REJIST; STATUS)
*  Description:
*     Select rows from a catalogue according to some criterion.
*  Arguments:
*     EI  =  INTEGER (Given)
*        Expression identifier for the selection criterion.
*     REJFLG  =  LOGICAL (Given)
*        Flag indicating whether or not a list of rejected rows
*        is to be created.  It is coded as follows:
*        .TRUE.  - create the list of rejected rows,
*        .FALSE. - do not create the list of rejected rows.
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     ROWS  =  INTEGER (Given)
*        Number of rows in the catalogue.
*     NUMSEL  =  INTEGER (Returned)
*        Number of rows selected.
*     SELIST(ROWS)  =  INTEGER (Returned)
*        List of selected rows.
*     NUMREJ  =  INTEGER (Returned)
*        Number of rejected rows.
*     REJIST(ROWS)  =  INTEGER (Returned)
*        List of rejected rows (optional).
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Initialise the number of selected and rejected rows.
*     For every row in the catalogue
*       Get the row.
*       Evaluate the expression for this row.
*       If the expression is true then
*         Increment the number of selected rows.
*         Add the current row to the list of selected rows.
*       else (the expression is false)
*         Increment the number of rejected rows.
*         If the list of rejected rows is to be produced then
*           Add the row to the list of rejected rows.
*         end if
*       end if
*     end for
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
*     14/4/94 (ACD): Original version.
*     3/5/94  (ACD): First stable version.
*     2/5/96  (ACD): Fixed bug in generating list of rejected rows.
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
     :  EI,
     :  CI,
     :  ROWS
      LOGICAL
     :  REJFLG
*  Arguments Returned:
      INTEGER
     :  NUMSEL,
     :  SELIST(ROWS),
     :  NUMREJ,
     :  REJIST(ROWS)
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      INTEGER
     :  ROWNO,   ! Current row number.
     :  CATROW   ! Current absolute catalogue row number.
      LOGICAL
     :  EVAL,    ! Result of evaluating the expression; true or false.
     :  NULFLG   ! Null value flag for evaluating the expression.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Initialise the number of selected and rejected rows.

         NUMSEL = 0
         NUMREJ = 0

*
*       Examine all the rows in the catalogue.

         DO ROWNO = 1, ROWS

*
*          Get the row.

            CALL CAT_RGET (CI, ROWNO, STATUS)

*
*          Evaluate the expression for this row.

            CALL CAT_EGT0L (EI, EVAL, NULFLG, STATUS)

*
*          If the expression is true then increment the number of
*          selected rows and add the row to the list of selected rows.
*
*          Remember that the row number may be the number in a
*          selection; convert it into an absolute row number.

            IF (EVAL) THEN
               NUMSEL = NUMSEL + 1

               CALL CAT1_CATRW (CI, ROWNO, CATROW, STATUS)
               SELIST(NUMSEL) = CATROW

            ELSE

*
*             The expression is false; increment the number of rejected
*             rows and, if appropriate, add the row to the list of
*             rejected rows (again, converting it to an absolute row
*             number).

               NUMREJ = NUMREJ + 1

               IF (REJFLG) THEN
                  CALL CAT1_CATRW (CI, ROWNO, CATROW, STATUS)
                  REJIST(NUMREJ) = CATROW
               END IF
            END IF
         END DO

      END IF

      END
