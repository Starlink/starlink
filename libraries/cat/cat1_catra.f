      SUBROUTINE CAT1_CATRA (GI, NUMROW, ROWNO, CATROW, STATUS)
*+
*  Name:
*     CAT1_CATRW
*  Purpose:
*     Convert an array row numbers to an array of absolute row numbers.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_CATRA (GI, NUMROW, ROWNO; CATROW; STATUS)
*  Description:
*     Convert an array row numbers in either a catalogue, a selection or
*     an index into an array of the corresponding absoluate catalogue row
*     numbers.  If the array already contains absolute catalogue row
*     numbers then the contents are copied unchanged.  However, if it
*     contains entries in a a selection or index then the corresponding
*     absolute catalogue row numbers are returned.
*  Arguments:
*     GI  =  INTEGER (Given)
*        Identifier to the entity to which the row corresponds; either
*        a catalogue, a selection or an index.
*     NUMROW  =  INTEGER (Given)
*        The number of row numbers.
*     ROWNO(NUMROW)  =  INTEGER (Given)
*        Array of row numbers in the catalogue, selection or index.
*     CATROW(NUMROW)  =  INTEGER (Returned)
*        Returned array of corresponding catalogue row numbers.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Attempt to determine the type of the identifier.
*     If ok then
*       If the identifier represents a catalogue then
*         Copy the row numbers.
*       else if the identifier represents a selection or an index then
*         Obtain the pointer to the list for this selection.
*         Convert the row numbers to absolute row numbers and copy them.
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
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     13/6/96  (ACD): Original version.
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
     :  NUMROW,
     :  ROWNO(NUMROW)
*  Arguments Returned:
      INTEGER
     :  CATROW(NUMROW)
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      INTEGER
     :  IDTYP,    ! Type of the supplied identifier.
     :  PTR,      ! Pointer to selection list.
     :  NUMSEL,   ! Number of objects in the selection.
     :  LOOP      ! Loop index.
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
               DO LOOP = 1, NUMROW
                  CATROW(LOOP) = ROWNO(LOOP)
               END DO

*
*          If the identifier represents a selection or an index then
*          obtain details of the selection and convert the row numbers
*          into absolute row numbers.

            ELSE IF (IDTYP .EQ. CAT__SITYP  .OR.  IDTYP .EQ. CAT__IITYP)
     :        THEN
               CALL CAT_TIQAI (GI, 'NUMSEL', NUMSEL, STATUS)
               CALL CAT_TIQAI (GI, 'PTR', PTR, STATUS)

               CALL CAT1_GTSRA (NUMSEL, %VAL(CNF_PVAL(PTR)),
     :                          NUMROW, ROWNO,
     :           CATROW, STATUS)

*
*          Report an error if the identifier was invalid.

            ELSE
               STATUS = CAT__INVID

               CALL CAT1_ERREP ('CAT1_CATRA_INVID', 'Identifier not '/
     :              /'a catalogue, selection or index.', STATUS)
            END IF

         END IF
*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            CALL CAT1_ERREP ('CAT1_CATRA_ERR', 'CAT1_CATRA: '/
     :        /'Error obtaining absolute catalogue row numbers.',
     :        STATUS)
         END IF

      END IF

      END
