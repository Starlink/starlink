      SUBROUTINE CAT_TROWS (CI, NUMROW, STATUS)
*+
*  Name:
*     CAT_TROWS
*  Purpose:
*     Get the number of rows in a catalogue, selection or index.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_TROWS (CI; NUMROW; STATUS)
*  Description:
*     Get the number of rows in a catalogue, selection or index.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue, selection or index identifier.
*     NUMROW  =  INTEGER (Returned)
*        Number of rows in the catalogue, selection or index.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Determine the type of the identifier.
*     If the identifier corresponds to a catalogue then
*       Attempt to find the array element corresponding to the
*       catalogue.
*       If ok then
*         Extract the number of rows from the catalogue arrays.
*       end if
*     else if the identifier corresponds to a selection or an index then
*       Get the number of rows in the selection by looking up the
*       appropriate attribute for the selection.
*     else
*       Set the status.
*       Report an error; the given identifier was of an appropriate
*       type.
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
*     17/7/93  (ACD): First implementation.
*     25/1/94  (ACD): Modified error reporting.
*     15/4/94  (ACD): Modified to handle selections.
*     28/11/94 (ACD): Modified to handle indices.
*     8/3/95   (ACD): Modified the handling of indices.
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
     :  CI
*  Arguments Returned:
      INTEGER
     :  NUMROW
*  Status:
      INTEGER STATUS   ! Global status.
*  Local Variables:
      INTEGER
     :  IDTYP,    ! Type of the given identifier.
     :  CIELM,    ! Array element corresponding to the catalogue.
     :  ERRLEN    ! Length of ERRTXT (excl. trail. blanks).
      CHARACTER
     :  ERRTXT*75 ! Text of error message.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Determine the type of the identifier and then handle the
*       various cases.

         CALL CAT_TIDTP (CI, IDTYP, STATUS)

         IF (IDTYP .EQ. CAT__CITYP) THEN

*
*          The identifier corresponds to a catalogue: attempt to find
*          the array element corresponding to the catalogue.  If
*          successful then proceed to extract the number of rows from
*          the catalogue arrays.

            CALL CAT1_CIELM (CI, CIELM, STATUS)

            IF (STATUS .EQ. CAT__OK) THEN
               NUMROW = NROW__CAT1(CIELM)
            END IF

         ELSE IF (IDTYP .EQ. CAT__SITYP  .OR.  IDTYP .EQ. CAT__IITYP)
     :     THEN

*
*          The identifier corresponds to a selection: attempt to get
*          the number of rows by looking up the appropriate
*          attribute.

            CALL CAT_TIQAI (CI, 'NUMSEL', NUMROW, STATUS)

         ELSE

*
*          The identifier corresponds to neither a catalogue, a
*          selection nor an index.  Set the status and report an error.

            STATUS = CAT__INVID

            CALL CAT1_ERREP ('CAT_TROWS_INVID', 'The given '/
     :        /'identifier corresponds to neither a catalogue, '/
     :        /'a selection nor an index.', STATUS)

         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            ERRTXT = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('CAT_TROWS: error getting the number of '/
     :        /'rows in catalogue (id. = ', ERRTXT, ERRLEN)
            CALL CHR_PUTI (CI, ERRTXT, ERRLEN)
            CALL CHR_PUTC (').', ERRTXT, ERRLEN)

            CALL CAT1_ERREP ('CAT_TROWS_ERR', ERRTXT(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
