      SUBROUTINE CCD1_GETARG( POS, VALUE )
*+
*  Name:
*     CCD1_GETARG

*  Purpose:
*     To get the value of a command-line argument.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_GETARG( POS, VALUE )

*  Description:
*     Call the GETARG intrinsic. Use so that GETARG may be called
*     from a C routine. The GETARG intrinsic is not always available
*     to a direct call from C (when intrinsic names are mangled).

*  Arguments:
*     POS = INTEGER (Given)
*        The argument required. The first argument is 0 and is usually
*        the program name.
*     VALUE = CHARACTER * ( * ) (Returned)
*        The value of the argument. Blank if no such argument
*        is available.

*  Notes:
*     Potentially not portable. GETARG is an extension.

*  Copyright:
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
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
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-JUL-2005 (PDRAPER):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER POS

*  Arguments Returned:
      CHARACTER * ( * ) VALUE

*  Local Variables:

*.
      CALL GETARG( POS, VALUE )
      END
