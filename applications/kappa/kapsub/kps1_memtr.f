      SUBROUTINE KPS1_MEMTR( N, FROM, IN, TO, OUT )
*+
*  Name:
*     KPS1_MEMTR

*  Purpose:
*     Transfers data from one array to another.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_MEMTR( N, FROM, IN, TO, OUT )

*  Description:
*     Transfers N elements from the vector IN, starting with element
*     FROM, to the vector OUT, starting at element TO.

*  Arguments:
*     N = INTEGER (Given)
*        Number of elements to transfer.
*     FROM = INTEGER (Given)
*        Index of first element of IN to be copied.
*     IN( * ) = REAL (Given)
*        The input array.
*     TO = INTEGER (Given)
*        The index at which the first transferred element should be
*        stored in OUT.
*     OUT( * ) = REAL (Given and Returned)
*        The output array.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-MAR-1995 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Arguments Given:
      INTEGER N
      INTEGER FROM
      REAL IN( * )
      INTEGER TO

*  Arguments Given and Returned:
      REAL OUT( * )

*  Local Variables:
      INTEGER OFFSET
*.

      OFFSET = TO - FROM

      DO I = FROM, FROM + N - 1
         OUT( OFFSET + I ) = IN( I )
      END DO

      END
