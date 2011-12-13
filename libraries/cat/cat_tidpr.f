      SUBROUTINE CAT_TIDPR (GI, CI, STATUS)
*+
*  Name:
*     CAT_TIDPR
*  Purpose:
*     Determine the parent to a component.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_TIDPR (GI; CI; STATUS)
*  Description:
*     Determine the parent to a component.  That is, the routine is
*     give the identifier of a component and it will return the
*     identifier of that component's parent catalogue.
*
*     If the routine is given a catalogue identifier it will return
*     the null identifier, because catalogues do not have parents.
*  Arguments:
*     GI  =  INTEGER (Given)
*        Identifier to a component.
*     CI  =  INTEGER (Returned)
*        Identifier to the parent catalogue of the component.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the identifier is greater than zero and less or equal to the
*     total number of identifiers then
*       Get the parent of the identifier.
*     else
*       Set the return status.
*       Set the parent identifier to null.
*     end if
*     Report any errors.
*  Implementation Deficiencies:
*     When joins are implemented it will probably be possible give the
*     routine a catalogue identifier and get it to return the identifier
*     of a join in which it is involved.
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
*     6/7/93  (ACD): Original version.
*     24/1/94 (ACD): Modified error reporting.
*     7/3/95  (ACD): Re-written, utilising the fact that the identifier
*       can be used as an array index.
*     11/4/95 (ACD): Changed the name of the null identifier.
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
      INCLUDE 'CAT1_IDS_CMN'      ! Identifiers common block.
*  Arguments Given:
      INTEGER
     :  GI
*  Arguments Returned:
      INTEGER
     :  CI
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      CHARACTER
     :  ERRTXT*75 ! Text of error message.
      INTEGER
     :  ERRLEN    ! Length of ERRTXT (excl. trail. blanks).
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Check that the given identifier lies in the current range for
*       valid identifiers.

         IF (GI .GT. 0  .AND.  GI .LE. NIDS__CAT1) THEN

*
*          The identifier is ok; get the corresponding parent
*          identifier.

            CI = IDPRN__CAT1(GI)

         ELSE

*
*          The identifier is illegal; set the status and set the
*          parent identifier to the null identifier.

            STATUS = CAT__NOCMP
            CI = CAT__NOID
         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            ERRTXT = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('CAT_TIDPR: error finding the parent of '/
     :        /'component (input identifier: ', ERRTXT, ERRLEN)
            CALL CHR_PUTI (GI, ERRTXT, ERRLEN)
            CALL CHR_PUTC (').', ERRTXT, ERRLEN)

            CALL CAT1_ERREP ('CAT_TIDPR_ERR', ERRTXT(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
