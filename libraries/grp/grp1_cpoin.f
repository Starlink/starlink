      SUBROUTINE GRP1_CPOIN( POINT, STATUS )
*+
*  Name:
*     GRP1_CPOIN

*  Purpose:
*     Convert a pointer to a UNIX character array descriptor into a
*     pointer to the area of mapped memory which holds the characters.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_CPOIN( POINT, STATUS )

*  Description:
*     UNIX does not use descriptors for character arrays, so just
*     return the supplied pointer unchanged.

*  Arguments:
*     POINT = INTEGER (Given and Returned)
*        On input, this is a pointer to a VMS character array
*        descriptor obtained using GRP1_CDESC. On exit, it is unchanged.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-AUG-1992 (DSB):
*        Original version
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given and Returned:
      INTEGER POINT

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Return immediately without doing anything.
      END
