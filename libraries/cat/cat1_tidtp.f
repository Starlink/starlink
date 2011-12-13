      SUBROUTINE CAT1_TIDTP (GI, IDTYP, STATUS)
*+
*  Name:
*     CAT1_TIDTP
*  Purpose:
*     Determine the type of a component.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_TIDTP (GI; IDTYP; STATUS)
*  Description:
*     Determine the type of a component.  The possibilities are:
*     catalogue, column (or field), parameter, expression, index,
*     selection or join.
*
*     Note that this internal routine deliberately does not report
*     any error.
*  Arguments:
*     GI  =  INTEGER (Given)
*        Identifier to a component.
*     IDTYP  =  INTEGER (Returned)
*        Type of the identifier, coded according to the following
*        scheme:
*        CAT__CITYP - catalogue,
*        CAT__FITYP - column (or field),
*        CAT__QITYP - parameter,
*        CAT__EITYP - expression,
*        CAT__IITYP - index,
*        CAT__SITYP - selection,
*        CAT__JITYP - join.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the identifier is greater than zero and less or equal to the
*     total number of identifiers then
*       Get the type of the identifier.
*     else
*       Set the return status.
*       Set the parent identifier to null.
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
*     19/9/99 (ACD): Original version (from CAT_TIDTP).
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
     :  IDTYP
*  Status:
      INTEGER STATUS              ! Global status.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Check that the given identifier lies in the current range for
*       valid identifiers.

         IF (GI .GT. 0  .AND.  GI .LE. NIDS__CAT1) THEN

*
*          The identifier is ok; get its type.

            IDTYP = IDTYP__CAT1(GI)

         ELSE

*
*          The identifier is illegal; set the status and set the type
*          to zero.

            STATUS = CAT__NOCMP
            IDTYP = 0
         END IF

      END IF

      END
