      SUBROUTINE IRM_CPOIN( POINT, STATUS )
*+
*  Name:
*     IRM_CPOIN

*  Purpose:
*     Convert a pointer to a character array descriptor into a
*     pointer to the area of mapped memory which holds the characters.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_CPOIN( POINT, STATUS )

*  Description:
*     This routine performs the opposite function to IRM_CDESC, i.e.
*     it converts a pointer to a character array descriptor into a
*     pointer to the memory used to store the characters. The dynamic
*     memory used to hold the character string descriptor (if any) is
*     released, but the memory used to store the actual characters is
*     not released.

*  Arguments:
*     POINT = INTEGER (Given and Returned)
*        On input, this is a pointer to a character array
*        descriptor obtained using IRM_CDESC. On exit, it is a pointer
*        to the area of memory containing the actual character data.
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

*  Arguments Given and Returned:
      INTEGER POINT

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Return immediately without doing anything.
      END
