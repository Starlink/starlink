      INTEGER FUNCTION CCD1_IARGC()
*+
*  Name:
*     CCD1_IARGC

*  Purpose:
*     Get the number of command-line arguments.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     INTEGER = CCD1_IARG()

*  Description:
*     Invoke the IARGC intrinsic. Use so that IARGC may be called
*     from a C routine. The IARGC intrinsic is not always available
*     to a direct call from C (when intrinsic names are mangled).

*  Notes:
*     Potentially not portable. IARGC is an extension.

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
      INTEGER IARGC

*.
      CCD1_IARGC = IARGC()
      END
