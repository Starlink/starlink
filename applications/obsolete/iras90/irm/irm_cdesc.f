      SUBROUTINE IRM_CDESC( LEN, POINT, STATUS )
*+
*  Name:
*     IRM_CDESC

*  Purpose:
*     Convert a pointer to an area of mapped memory into a pointer to a
*     character array descriptor.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_CDESC( LEN, POINT, STATUS )

*  Description:
*     The supplied pointer locates a section of memory to be used as a
*     character array. This routine constructs a descriptor for the
*     character array and replaces the supplied pointer with a pointer
*     to this descriptor. The returned pointer can then be passed to a
*     subroutine using the %VAL construct, and the corresponding dummy
*     argument can be declared as a passed length character array in
*     the normal way.
*
*     IRM_CPOIN should be called to un-do the effect of this routine
*     before releasing the mapped memory.

*  Arguments:
*     LEN = INTEGER (Given)
*        The number of characters in each element of the array.
*     POINT = INTEGER (Given and Returned)
*        On input, this is a pointer to an area of memory mapped using
*        (for instance) PSX_CALLOC. On exit, it is a pointer to a
*        character string descriptor.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     30-APR-1992 (DSB):
*        Original version.
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
