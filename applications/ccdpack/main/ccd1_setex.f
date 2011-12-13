      SUBROUTINE CCD1_SETEX( NULL, NL, STATUS )
*+
*  Name:
*     CCD1_SETEX

*  Purpose:
*     Return the current parameter system null symbol (shriek).

*  Language:
*     Fortran-77.

*  Invocation:
*     CALL CCD1_SETEX( NULL, NL, STATUS )

*  Description:
*     This routine returns, as a character string, the current symbol
*     used as the NULL value. Currently this is set to "!" for all
*     systems except IRAF, which uses the string "INDEF".
*
*     This routine is for use in constructing output messages which
*     advise on the use of the NULL symbol.

*  Arguments:
*     NULL = CHARACTER * ( * ) (Returned)
*        The current NULL symbol. This should be at least 5 characters big.
*     NL = INTEGER (Returned)
*        The number of characters in NULL.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1997 Central Laboratory of the Research Councils.
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
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     7-JUL-1997 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard parameters
      INCLUDE 'PSX_ERR'         ! PSX error codes.

*  Arguments Returned:
      CHARACTER * ( * ) NULL
      INTEGER NL

*  Status:
      INTEGER STATUS            ! Global status

*  Local Variables:
      CHARACTER * ( 132 ) VALUE ! Value of environment variable.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  To test which environment we're running under we use the existence of
*  the "ccdpack$" variable. This is defined as part of the setup.
      CALL ERR_MARK
      CALL PSX_GETENV( 'ccdpack', VALUE, STATUS )
      IF ( STATUS .EQ. PSX__NOENV ) THEN
         CALL ERR_ANNUL( STATUS )
         NULL = '!'
         NL = 1
      ELSE
         NULL = 'INDEF'
         NL = 5
      END IF
      CALL ERR_RLSE
      END
* $Id$
