      SUBROUTINE CAT_TIDTP (GI, IDTYP, STATUS)
*+
*  Name:
*     CAT_TIDTP
*  Purpose:
*     Determine the type of a component.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_TIDTP (GI; IDTYP; STATUS)
*  Description:
*     Determine the type of a component.  The possibilities are:
*     catalogue, column (or field), parameter, expression, index,
*     selection or join.
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
*     Determine the type of the component.
*     Report any error.
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
*     13/7/93 (ACD): Original version.
*     24/1/94 (ACD): Modified error reporting.
*     7/3/95  (ACD): Re-written, utilising the fact that the identifier
*       can be used as an array index.
*     19/9/99 (ACD): Moved determining the type of the component to
*       another, internal, routine, leaving just the error reporting
*       in this one.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
*  Arguments Given:
      INTEGER
     :  GI
*  Arguments Returned:
      INTEGER
     :  IDTYP
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
*       Determine the type of the component.

         CALL CAT1_TIDTP (GI, IDTYP, STATUS)

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            ERRTXT = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('CAT_TIDTP: error finding the type of a '/
     :        /'component (input identifier: ', ERRTXT, ERRLEN)
            CALL CHR_PUTI (GI, ERRTXT, ERRLEN)
            CALL CHR_PUTC (').', ERRTXT, ERRLEN)

            CALL CAT1_ERREP ('CAT_TIDTP_ERR', ERRTXT(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
