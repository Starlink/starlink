      SUBROUTINE CAT1_CATRW (GI, ROWNO, CATROW, STATUS)
*+
*  Name:
*     CAT1_CATRW
*  Purpose:
*     Convert a row number to an absolute catalogue row number.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_CATRW (GI, ROWNO; CATROW; STATUS)
*  Description:
*     Convert a row number in either a catalogue, a selection or an
*     index into the corresponding catalogue row number.  If the row
*     number already corresponds to a catalogue it is unchanged.  If it
*     corresponds to a selection or index then the corresponding
*     catalogue row number is returned.
*  Arguments:
*     GI  =  INTEGER (Given)
*        Identifier to the entity to which the row corresponds; either
*        a catalogue, a selection or an index.
*     ROWNO  =  INTEGER (Given)
*        Given row number in the catalogue, selection or index.
*     CATROW  =  INTEGER (Returned)
*        Returned corresponding catalogue row number.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Attempt to determine the type of the identifier.
*     If ok then
*       If the identifier represents a catalogue then
*         Copy the row number.
*       else if the identifier represents a selection or an index then
*         Obtain the pointer to the list for this selection.
*         Obtain the corresponding catalogue row from the list.
*       else
*         Report an error.
*       end if
*     end if
*     If any error occurred then
*       Report it.
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
*     14/4/94  (ACD): Original version.
*     3/5/94   (ACD): First stable version.
*     28/11/94 (ACD): Added handling of indices.
*     8/3/95   (ACD): Modified the handling of indices.
*     13/6/96  (ACD): Removed INCLUDE for unused common block.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CNF_PAR'           ! For CNF_PVAL function
*  Arguments Given:
      INTEGER
     :  GI,
     :  ROWNO
*  Arguments Returned:
      INTEGER
     :  CATROW
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      INTEGER
     :  IDTYP,    ! Type of the supplied identifier.
     :  PTR,      ! Pointer to selection list.
     :  NUMSEL,   ! Number of objects in the selection.
     :  ERRLEN    ! Length of ERRTXT (excl. trail. blanks).
      CHARACTER
     :  ERRTXT*75 ! Text of error message.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Attempt to determine the type of the identifier and proceed if
*       ok.

         CALL CAT_TIDTP (GI, IDTYP, STATUS)

         IF (STATUS .EQ. CAT__OK) THEN

*
*          If the identifier represents a catalogue simply copy the
*          row number.

            IF (IDTYP .EQ. CAT__CITYP) THEN
               CATROW = ROWNO

*
*          If the identifier represents a selection or an index then
*          look up the number of rows, check that the given row
*          number lies in the range for the selection or index and
*          finally look up the corresponding absolute row number.

            ELSE IF (IDTYP .EQ. CAT__SITYP  .OR.  IDTYP .EQ. CAT__IITYP)
     :        THEN
               CALL CAT_TIQAI (GI, 'NUMSEL', NUMSEL, STATUS)

               IF (ROWNO .GE. 1  .AND.  ROWNO .LE. NUMSEL) THEN
                  CALL CAT_TIQAI (GI, 'PTR', PTR, STATUS)
                  CALL CAT1_GTSRW (NUMSEL, %VAL(CNF_PVAL(PTR)),
     :                             ROWNO, CATROW,
     :              STATUS)
               ELSE
                  STATUS = CAT__INVRW

                  ERRTXT = ' '
                  ERRLEN = 0

                  CALL CHR_PUTC ('CAT1_CATRW: Invalid row number (',
     :              ERRTXT, ERRLEN)
                  CALL CHR_PUTI (ROWNO, ERRTXT, ERRLEN)
                  CALL CHR_PUTC (') in selection or index.',
     :              ERRTXT, ERRLEN)

                  CALL CAT1_ERREP ('CAT1_CATRW_INVRS',
     :              ERRTXT(1 : ERRLEN), STATUS)
               END IF

*
*          Report an error if the identifier was invalid.

            ELSE
               STATUS = CAT__INVID

               CALL CAT1_ERREP ('CAT1_CATRW_INVID', 'Identifier not '/
     :              /'a catalogue, selection or index.', STATUS)
            END IF

         END IF
*
*       If any error occurred then report it and set the absolute
*       row number to 1.

         IF (STATUS .NE. CAT__OK) THEN
            CATROW = 1

            CALL CAT1_ERREP ('CAT1_CATRW_ERR', 'CAT1_CATRW: '/
     :        /'Error obtaining absolute catalogue row number.', STATUS)
         END IF

      END IF

      END
