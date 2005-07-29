      SUBROUTINE NDF1_FARG( INDEX, VALUE )
*+
* Name:
*    NDF1_FARG

*  Purpose:
*    Get a command-line argument from the Fortran routine GETARG.

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
