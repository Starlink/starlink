      CHARACTER*15 FUNCTION SUBPAR_CTYPE( TYPE )
*+
*  Name:
*     SUBPAR_CTYPE

*  Purpose:
*     To return the parameter type as a string

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = SUBPAR_CTYPE( TYPE )

*  Description:
*     The function value is set to the character form of a primitive
*     type, or to 'NON-PRIMITIVE'

*  Arguments:
*     TYPE = INTEGER (Given)
*        The TYPE code to be interpreted

*  Returned Value:
*     SUBPAR_CTYPE = CHARACTER * ( 15 )
*        The character form of the type.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
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
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-DEC-1992 (AJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'SUBPAR_PAR'       ! SUBPAR constants

*  Arguments Given:
      INTEGER TYPE
*.

      IF ( TYPE .EQ. SUBPAR__CHAR ) THEN
         SUBPAR_CTYPE = '_CHAR'
      ELSEIF ( TYPE .EQ. SUBPAR__DOUBLE ) THEN
         SUBPAR_CTYPE = '_DOUBLE'
      ELSEIF ( TYPE .EQ. SUBPAR__INTEGER ) THEN
         SUBPAR_CTYPE = '_INTEGER'
      ELSEIF ( TYPE .EQ. SUBPAR__LOGICAL ) THEN
         SUBPAR_CTYPE = '_LOGICAL'
      ELSEIF ( TYPE .EQ. SUBPAR__REAL) THEN
         SUBPAR_CTYPE = '_REAL'
      ELSE
         SUBPAR_CTYPE = 'NON-PRIMITIVE'
      ENDIF

      END
