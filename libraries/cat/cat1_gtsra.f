      SUBROUTINE CAT1_GTSRA (NUMSEL, SELIST, NUMROW, ROWNO, CATROW,
     :  STATUS)
*+
*  Name:
*     CAT1_GTSRA
*  Purpose:
*     Convert selection row numbers into absolute row numbers.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_GTSRA (NUMSEL, SELIST, NUMROW, ROWNO; CATROW;
*       STATUS)
*  Description:
*     Convert an array or row numbers in a selection or index into
*     absolute row numbers.
*  Arguments:
*     NUMSEL  =  INTEGER (Given)
*        Number of rows in the selection.
*     SELIST(NUMSEL)  =  INTEGER (Given)
*        The selection list; the list of catalogue rows which comprise
*        the selection.
*     NUMROW  =  INTEGER (Given)
*        Number of rows in the selection.
*     ROWNO(NUMROW)  =  INTEGER (Given)
*        Selection row numbers.
*     CATROW(NUMROW)  =  INTEGER (Returned)
*        Corresponding catalogue row numbers.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Check that the given row numbers are in range and if so copy the
*     corresponding catalogue row numbers from the appropriate element
*     of the selection list.
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
*     13/6/96 (ACD): Original version.
*     14/6/96 (ACD): First stable version.
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
     :  NUMSEL,
     :  SELIST(NUMSEL),
     :  NUMROW,
     :  ROWNO(NUMROW)
*  Arguments Returned:
      INTEGER
     :  CATROW(NUMROW)
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      INTEGER
     :  LOOP,     ! Loop index.
     :  CURSEL,   ! Current row in selection or index.
     :  ERRLEN    ! Length of ERRTXT (excl. trail. blanks).
      CHARACTER
     :  ERRTXT*75 ! Text of error message.
*.

      IF (STATUS .EQ. CAT__OK) THEN

         DO LOOP = 1, NUMROW
            CURSEL = ROWNO(LOOP)

            IF (CURSEL .GE. 1  .AND.  CURSEL .LE. NUMSEL) THEN
               CATROW(LOOP) = SELIST(CURSEL)

            ELSE
               CATROW(LOOP) = 1

               STATUS = CAT__INVRW

               ERRTXT = ' '
               ERRLEN = 0

               CALL CHR_PUTC ('CAT1_GTSRA: Invalid row number (',
     :           ERRTXT, ERRLEN)
               CALL CHR_PUTI (CURSEL, ERRTXT, ERRLEN)
               CALL CHR_PUTC (') in selection or index.', ERRTXT,
     :           ERRLEN)

               CALL CAT1_ERREP ('CAT1_GTSRA_INVRS',
     :           ERRTXT(1 : ERRLEN), STATUS)
            END IF
         END DO

      END IF

      END
