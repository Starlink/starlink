      SUBROUTINE CAT_RGET (CI, ROWNO, STATUS)
*+
*  Name:
*     CAT_RGET
*  Purpose:
*     Read a specified row into the current row buffer.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_RGET (CI, ROWNO; STATUS)
*  Description:
*     Read a specified row from a catalogue, selection or index into
*     the current row buffer for that catalogue.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue, selection or index identifier.  The row number,
*        ROWNO (below), refers to the row number in the catalogue,
*        selection or index, as appropriate.
*     ROWNO  =  INTEGER (Given)
*        Number of the row to be read.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     This routine justs sets the row number for the current row in
*     the appropriate common block array.  The actual manipulation of
*     the current row buffer is done in each back-end.
*
*     Ensure that the given row corresponds to a row in the catalogue
*     by converting a row in a selection or index into the corresponding
*     catalogue row.
*     Attempt to get the array element corresponding to the catalogue.
*     If ok then
*       If the requested row is inside the range of rows for the
*       catalogue then
*         Set the current row to the requested row.
*       else
*         Set the error status: invalid row number.
*       end if
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
*     13/7/93  (ACD): First implementation.
*     24/1/94  (ACD): Modified error reporting.
*     14/4/94  (ACD): Added handling of selections.
*     28/11/94 (ACD): Added handling of indices.
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
     :  CI,
     :  ROWNO
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  CIELM,    ! Array element corresponding to the catalogue.
     :  CATROW,   ! Requested row in the catalogue.
     :  ERRLEN    ! Length of ERRMSG (excl. trail. blanks).
      CHARACTER
     :  ERRMSG*85 ! Error message.
*.

C     write(17, 1000) rowno, status
C1000 format(1x, 'RGET on entry - rowno, status: ', I5, i10 )

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Obtain the requested row as an absolute row in the catalogue.
*       If the given identifier corresponds to a catalogue it will
*       already be in this form.  If it corresponds to a selection or
*       index then the corresponding row must be found.

         CALL CAT1_CATRW (CI, ROWNO, CATROW, STATUS)

C        print2000, rowno, catrow, status
C2000    format(1x, 'After CAT1_CATRW, rowno, catrow, status: ',
C    :     i10, i10, i20)

*
*       Attempt to get the array element corresponding to the catalogue.
*       and proceed if ok.

         CALL CAT1_CIELM (CI, CIELM, STATUS)

         IF (STATUS .EQ. CAT__OK) THEN

*
*          Check that the requested row falls within the range of
*          rows for the catalogue.

            IF (CATROW .GE. 1  .AND.  CATROW .LE. NROW__CAT1(CIELM) )
     :        THEN

*
*             Set the current row to the requested row.

               CROW__CAT1(CIELM) = CATROW

C              write(17, 1002) cielm, nrow__cat1(cielm)
C1002          format(1x, 'cielm, nrow__cat1(cielm): ', I6, I6 )

            ELSE
               STATUS = CAT__INVRW

            END IF

         END IF

*
*       Report any error.
*       Note that if the status returned corresponds to an attempt to
*       access an invalid row no error is reported.

         IF ((STATUS .NE. CAT__OK) .AND. (STATUS .NE. CAT__INVRW)) THEN
            ERRLEN = 0
            ERRMSG = ' '

            CALL CHR_PUTC ('CAT_RGET: error reading row ', ERRMSG,
     :        ERRLEN)
            CALL CHR_PUTI (ROWNO, ERRMSG, ERRLEN)

            IF (CATROW .NE. ROWNO) THEN
               CALL CHR_PUTC ('(', ERRMSG, ERRLEN)
               CALL CHR_PUTI (CATROW, ERRMSG, ERRLEN)
               CALL CHR_PUTC (')', ERRMSG, ERRLEN)
            END IF

            CALL CHR_PUTC (' from catalogue.', ERRMSG, ERRLEN)

            CALL CAT1_ERREP ('CAT_RGET_ERR', ERRMSG(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

C     write(17, 1001) rowno, status
C1001 format(1x, 'RGET on exit - rowno, status: ', I5, i10 )

      END
