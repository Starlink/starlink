      SUBROUTINE SUBPAR_DEF0L ( NAMECODE, LVALUE, STATUS )
      IMPLICIT NONE

*+
*  Name:
*     SUBPAR_DEF0L

*  Purpose:
*     Set dynamic default scalar value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_DEF0L ( NAMECODE, VALUE, STATUS )

*  Description:
*     This routine sets a default value for a scalar primitive object
*     associated with a Parameter.
*     There is a routine for each access type, LOGICAL:

*        SUBPAR_DEF0D    DOUBLE PRECISION
*        SUBPAR_DEF0R    REAL
*        SUBPAR_DEF0I    INTEGER
*        SUBPAR_DEF0L    LOGICAL
*        SUBPAR_DEF0C    CHARACTER[*n]

*     If the object data type differs from the access type, LOGICAL, then
*     conversion is performed (if allowed).

*  Arguments:
*     NAMECODE=INTEGER (given)
*        code-number of the parameter
*     LVALUE=LOGICAL
*        Expression specifying the default value for the object.
*     STATUS=INTEGER

*  Algorithm:
*     Use SUBPAR_DEF1L.

*  Copyright:
*     Copyright (C) 1984, 1985, 1987, 1992 Science & Engineering Research Council.
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
*     BDK: B D Kelly (ROE)
*     {enter_new_authors_here}

*  History:
*     10-DEC-1984 (BDK):
*        Original
*     05-JUN-1985 (BDK):
*        Set PARDYN(2,NAMECODE) values
*     09-NOV-1987 (BDK):
*        Use SUBPAR_DEF1L
*     09-MAR-1992 (AJC):
*        Use the special flag NVAL = 0 in calling _DEF1L
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Global Constants:
      INCLUDE 'SAE_PAR'


*  Arguments Given:
      INTEGER NAMECODE                  ! Parameter code number

      LOGICAL LVALUE			! Scalar to supply value

*    Status return :
      INTEGER STATUS			! Status Return


*  Local Variables:
      INTEGER NVAL                      ! number of values

*.


      IF (STATUS .NE. SAI__OK) RETURN

*
*   set the number of values and call SUBPAR_DEF1L.
*   NVAL = 0 has special meaning i.e a scalar value
*
      NVAL = 0
      CALL SUBPAR_DEF1L ( NAMECODE, NVAL, LVALUE, STATUS )

      END
