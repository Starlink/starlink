      SUBROUTINE UPUT( J, K, N )
*+
*  Name:
*     UPUT

*  Purpose:
*     Writes a block to an external file for MEMSYS3.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL UPUT( J, K, N )

*  Description:
*     This subroutine is called directly by the MEMSYS3 library.  It
*     transfers N elements from the internal work arrays held in common
*     block /MECOMS/ to external storage.  The external storage is
*     assumed to be in the form of a continuous 1-d vector.  The given
*     argument J is the index within this external storage vector at
*     which the first transferred element (element K of the internal
*     work array ME_ST) should be stored.

*  Arguments:
*     J = INTEGER (Given)
*        The index within the external storage vector at which
*        to store the first element transfered.
*     K = INTEGER (Given)
*        The index within the internal work array of the first element
*        to be transferred.
*     N = INTEGER (Given)
*        The number of elements to be transferred.

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

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Variables:
      INCLUDE 'ME_COM'           ! MEMSYS3 common blocks
      INCLUDE 'C1_COM'           ! Common blocks used to communicate
                                 ! with MEM2D.
*  Arguments Given:
      INTEGER J
      INTEGER K
      INTEGER N
*.

      CALL KPS1_MEMTR( N, K, ME_ST, J, %VAL( C1_IP0 ) )

      END
