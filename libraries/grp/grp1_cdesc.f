      SUBROUTINE GRP1_CDESC( LEN, POINT, STATUS )
*+
*  Name:
*     GRP1_CDESC

*  Purpose:
*     Convert a pointer to an area of mapped memory into a pointer to a
*     UNIX character array descriptor.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_CDESC( LEN, POINT, STATUS )

*  Description:
*     UNIX does not use descriptors, so just return the pointer
*     unchanged.

*  Arguments:
*     LEN = INTEGER (Given)
*        The number of characters in each element of the array.
*     POINT = INTEGER (Given and Returned)
*        On input, this is a pointer to an area of memory mapped using
*        On exit, it is unchanged.
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

*  Arguments Given:
      INTEGER LEN

*  Arguments Given and Returned:
      INTEGER POINT

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Return without doing anything.
      END
