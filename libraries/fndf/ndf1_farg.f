      SUBROUTINE NDF1_FARG( INDEX, VALUE )
*+
*  Name:
*     NDF1_FARG

*  Purpose:
*     Get a command-line argument from the Fortran routine GETARG.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_FARG( INDEX, VALUE )

*  Description:
*     Gets the INDEX-th command-line argument from Fortran using the
*     non-standard intrinsic GETARG. We use this rather than calling
*     GETARG directly from the C routine ndf_gtarg.c as namespace
*     mangling means that Fortran intrinsics are not always available
*     from C.

*  Arguments:
*     INDEX = INTEGER (Given)
*        The index of the argument to return.
*     VALUE = CHARACTER * ( * ) (Returned)
*        The value of the INDEX-th argument.

*  Copyright:
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council

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
*     PWD: Peter W. Draper (JAC, Durham University)
*     {enter_new_authors_here}

*  History:
*     29-JUL-2005 (PWD):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER INDEX

*  Arguments Returned:
      CHARACTER * ( * ) VALUE

*.

      CALL GETARG( INDEX, VALUE )
      END
