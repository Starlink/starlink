      CHARACTER*15 FUNCTION SUBPAR_PARGP( NAMECODE )
*+
*  Name:
*     SUBPAR_PARGP

*  Purpose:
*     To return the parameter group name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = SUBPAR_PARGP( NAMECODE )

*  Description:
*     The function value is set to 'Pnnn', where nnn is the character
*     representation of the given namecode. This will be unique for
*     each parameter in a task or monolith

*  Arguments:
*     NAMECODE = INTEGER (Given)
*        The parameter namecode

*  Returned Value:
*     SUBPAR_PARGP = CHARACTER * ( 15 )
*        The HDS locator group name for this parameter.

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils.
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
*      3-FEB-2000 (AJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS constants

*  Arguments Given:
      INTEGER NAMECODE

*  Local Variables:
      CHARACTER*(DAT__SZGRP) GRPNM
*.

      GRPNM = 'P'
      WRITE( GRPNM(2:), '(I4.4)' ) NAMECODE

      SUBPAR_PARGP = GRPNM

      END
