      SUBROUTINE KPS1_MEMFX( SIZE, LAST, DATA )
*+
*  Name:
*     KPS1_MEMFX

*  Purpose:
*     Fixes an external MEMSYS3 area.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_MEMFX( SIZE, LAST, DATA )

*  Description:
*     Fills the section of an external area which lies beyond the end
*     of the actual image with zeros.

*  Arguments:
*     SIZE = INTEGER (Given)
*        Total number of elements in the data file.
*     LAST = INTEGER (Given)
*        The index of the last genuine data value.
*     DATA( SIZE ) = REAL (Given and Returned)
*        The data array.  Elements with indices higher than LAST will
*        be set to zero on return.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-MAR-1995 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Arguments Given:
      INTEGER SIZE
      INTEGER LAST

*  Arguments Given and Returned:
      REAL DATA( SIZE )

*  Local Variables:
      INTEGER I

*.

      DO I = LAST + 1, SIZE
         DATA( I ) = 0.0
      END DO

      END
