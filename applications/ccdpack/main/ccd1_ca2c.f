      SUBROUTINE CCD1_CA2C( DATA, ILINE, LINE, STATUS )
*+
*  Name:
*     CCD1_CA2C

*  Purpose:
*     Copy AST_ data from an HDS object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_CA2C( DATA, ILINE, LINE, STATUS )

*  Description:
*     This routine copies a line of text representing AST_ data from a
*     specified element of a 1-dimensional character array. It is
*     intended for use when reading AST_ data from an HDS object (i.e
*     an HDS _CHAR array).

*  Arguments:
*     DATA( * ) = CHARACTER * ( * ) (Given)
*        The character array from which the text is to be copied.
*     ILINE = INTEGER (Given)
*        The index of the element in DATA which is to provide the text
*        (the contents of other elements are ignored).
*     LINE = CHARACTER * ( * ) (Returned)
*        The line of text obtained.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-FEB-1998 (DSB):
*        Original version, based on NDF1_H2AST.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) DATA( * )
      INTEGER ILINE

*  Arguments Returned:
      CHARACTER * ( * ) LINE

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract the required line of text.
      LINE = DATA( ILINE )

      END
