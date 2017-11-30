      SUBROUTINE NDF1_ASETC( IAST, VALUE, ATTRIB, STATUS )
*+
*  Name:
*     NDF1_ASETC

*  Purpose:
*     Set an AST_ character attribute value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_ASETC( IAST, VALUE, ATTRIB, STATUS )

*  Description:
*     The routine calls AST_SETC to assign a character attribute value
*     to an AST_ Object. It is a simple wrap-up of this AST_ routine
*     with the character argument order swapped. This is done so that
*     mapped character data may be used for the attribute value.

*  Arguments:
*     IAST = INTEGER (Given)
*        Pointer to the AST_ Object.
*     VALUE = CHARACTER * ( * ) (Given)
*        The attribute vakue to be assigned.
*     ATTRIB = CHARACTER * ( * ) (Given)
*        The attribute name.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - Trailing blanks are removed from the attribute value before
*     use.

*  Copyright:
*     Copyright (C) 1997 Rutherford Appleton Laboratory

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     14-JUL-1997 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST_ public interface

*  Arguments Given:
      INTEGER IAST
      CHARACTER * ( * ) VALUE
      CHARACTER * ( * ) ATTRIB

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Variables:
      INTEGER L                  ! Length of attribute value

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the significant length of the attribute value.
      L = MAX( 1, CHR_LEN( VALUE ) )

*  Assign the value.
      CALL AST_SETC( IAST, ATTRIB, VALUE( : L ), STATUS )

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_ASETC', STATUS )

      END
