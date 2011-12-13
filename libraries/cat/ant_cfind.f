      SUBROUTINE ANT_CFIND (TID, CNAME, CID, DTYPE, STATUS)
*+
*  Name:
*     ANT_CFIND
*  Purpose:
*     Find the identifier and datatype of a column or parameter.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL (TID, CNAME; CID, DTYPE; STATUS)
*  Description:
*     Find the identifier and datatype of a column, vector column
*     element or parameter.
*  Arguments:
*     TID  INTEGER (ENTRY)
*        Table (catalogue) identifier.
*     CNAME  CHARACTER*(*) (ENTRY)
*        Name of the column or parameter for which the identifier is
*        to be found.  Column names may be either scalars or elements
*        of vectors.  See the StarBase documentation for a
*        specificiation of the syntax of vector elements.
*     CID  INTEGER (EXIT)
*        Identifier to the column or parameter.
*     DTYPE  INTEGER (EXIT)
*        Data type of the column or parameter (coded as for the ANT
*        package).
*     STATUS  =  INTEGER (UPDATE)
*        The global status.
*  Prior Requirements:
*     <...>
*  Side Effects:
*     <...>
*  Algorithm:
*     Obtain the identifier for the column, vector column element or
*     parameter.
*     If an identifier is obtained successfully then
*       Obtain the data type for this column or parameter.
*       Convert the StarBase data type to the parser (ANT) data type.
C       If the item is a parameter then
C         Set its data type to double precision.
C       end if
*     end if
*  Implementation Deficiencies:
C     Setting all parameters to have a type of double precision is a
C     kludge.  It is necessary because in both ADC and FITS all keywords
C     turn out as type character.  The parser will not accept character
C     elements in arithmetric expressions.  Setting the data type to
C     double precision works because type conversion occurs.

*  Copyright:
*     Copyright (C) 1993, 1994 Science & Engineering Research Council.
*     All Rights Reserved.

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
*     10/8/93 (ACD): Original version.
*     12/8/93 (ACD): First stable version.
*     2/2/94  (ACD): Added vector column elements.
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
     :  TID
      CHARACTER
     :  CNAME*(*)
*  Arguments Returned:
      INTEGER
     :  CID,
     :  DTYPE
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  SBTYPE,  ! StarBase data type.
     :  idtype   ! Type of identifier: parameter or column.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Attempt to obtain an identifier for the column, vector column
*       element or parameter, and proceed if one was obtained
*       successfully.

         CALL CAT_TIDNT (TID, CNAME, CID, STATUS)

         IF (STATUS .EQ. CAT__OK) THEN

*
*          Obtain the data type of this column or parameter.

            CALL CAT_TIQAI (CID, 'DTYPE', SBTYPE, STATUS)

*
*          Convert the StarBase data type to the corresponding parser
*          data type.

            IF (SBTYPE .EQ. CAT__TYPEB) THEN
               DTYPE = 1

            ELSE IF (SBTYPE .EQ. CAT__TYPEW) THEN
               DTYPE = 2

            ELSE IF (SBTYPE .EQ. CAT__TYPEI) THEN
               DTYPE = 3

            ELSE IF (SBTYPE .EQ. CAT__TYPER) THEN
               DTYPE = 4

            ELSE IF (SBTYPE .EQ. CAT__TYPED) THEN
               DTYPE = 5

            ELSE IF (SBTYPE .EQ. CAT__TYPEL) THEN
               DTYPE = 0

            ELSE IF (SBTYPE .EQ. CAT__TYPEC) THEN
               DTYPE = -1

            END IF

C
C          If the item is a parameter then set its data type (as
C          understood by the parser) to double precision.
C          Welcome to Kludge City.  Please drive carefully; bugs on the
C          road ahead.  Or maybe its just a really neat trick.

            call cat_tidtp (cid, idtype, status)

            if (idtype .eq. cat__qityp) then
               dtype = 5
            end if

         END IF

      END IF

      END
