      SUBROUTINE CAT1_CIELM (GI, CIELM, STATUS)
*+
*  Name:
*     CAT1_CIELM
*  Purpose:
*     Get the array element equivalent to a catalogue identifier.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_CIELM (GI; CIELM; STATUS)
*  Description:
*     Get the element in the catalogue common blocks corresponding to
*     a given catalogue identifier.  If the routine is passed a
*     selection or index identifier then the array element for the
*     corresponding catalogue is returned.
*  Arguments:
*     GI  =  INTEGER (Given)
*        Catalogue, selection or index identifier.
*     CIELM  =  INTEGER (Returned)
*        Array element in the catalogue common blocks which hold values
*        corresponding to this catalogue.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the identifier is greater than zero and less than or equal to
*     the total number of identifiers then
*       Get the type of the identifier.
*       If the identifier corresponds to a catalogue then
*         Get the catalogue array element corresponding to the
*         identifier.
*       else if the identifier corresponds to a selection or an index
*       then
*         Get the catalogue array element corresponding to the parent
*         of the identifier.
*       else
*         Set the status.
*         Report an error.
*       end if
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
*     ACD: A C Davenhall (Leicester)
*  History:
*     24/6/93  (ACD): Original version.
*     23/1/94  (ACD): Modified error reporting.
*     14/4/94  (ACD): Added handling of selection identifiers.
*     28/11/94 (ACD): Added handling of index identifiers.
*     7/3/95   (ACD): Re-written to utilise the fact that the identifier
*       can be used as an array index.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
*  Global Variables:
      INCLUDE 'CAT1_IDS_CMN'      ! Identifiers common block.
*  Arguments Given:
      INTEGER
     :  GI
*  Arguments Returned:
      INTEGER
     :  CIELM
*  Status:
      INTEGER STATUS              ! Global status.
*  Local Variables:
      INTEGER
     :  IDTYP,    ! Identifier type.
     :  ERRLEN    ! Length of ERRTXT (excl. trail. blanks).
      CHARACTER
     :  ERRTXT*75 ! Text of error message.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Check that the identifier lies in the range currently permitted
*       for identifiers.

         IF (GI .GT. 0  .AND.  GI .LE. NIDS__CAT1) THEN

*
*          Get the type of the identifier.

            IDTYP = IDTYP__CAT1(GI)

*
*          If the identifier corresponds to a catalogue then get the
*          corresponding catalogue array element.

            IF (IDTYP .EQ. CAT__CITYP) THEN
               CIELM = IDCEL__CAT1(GI)

*
*          If the identifier corresponds to an index or a selection then
*          get the catalogue array element of the parent catalogue.

            ELSE IF (IDTYP .EQ. CAT__SITYP  .OR.  IDTYP .EQ. CAT__IITYP)
     :        THEN
               CIELM = IDCEL__CAT1( IDPRN__CAT1(GI) )

*
*          Otherwise the identifier is invalid for this operation;
*          set the return value to zero, set the status and report an
*          error.

            ELSE
               CIELM = 0

               STATUS = CAT__INVID

               ERRTXT = ' '
               ERRLEN = 0

               CALL CHR_PUTC ('CAT1_CIELM: Identifier ', ERRTXT, ERRLEN)
               CALL CHR_PUTI (GI, ERRTXT, ERRLEN)
               CALL CHR_PUTC ('is neither a catalogue, a selection '/
     :           /'nor an index.', ERRTXT, ERRLEN)

               CALL CAT1_ERREP ('CAT1_CIELM_INVID', ERRTXT(1 : ERRLEN),
     :           STATUS)

            END IF

         ELSE

*
*          The identifier lies outside the permitted range; set the
*          return value to zero, set the status and report an error.

            CIELM = 0

            STATUS = CAT__INVID

            ERRTXT = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('CAT1_CIELM: identifier out of range '/
     :        /'(value: ', ERRTXT, ERRLEN)
            CALL CHR_PUTI (GI, ERRTXT, ERRLEN)
            CALL CHR_PUTC (').', ERRTXT, ERRLEN)

            CALL CAT1_ERREP ('CAT1_CIELM_ERR', ERRTXT(1 : ERRLEN),
     :        STATUS)

         END IF

      END IF

      END
