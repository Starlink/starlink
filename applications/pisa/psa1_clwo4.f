      SUBROUTINE PSA1_CLWO4( INLINE, DESLIN, LIMIT, OFFSET, STATUS )
*+
*  Name:
*     PSA1_CLWO

*  Purpose:
*     To copy an INTEGER line of data to another line with an
*     offset

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PSA1_CLWO( INLINE, DESLIN, OFFSET, STATUS )

*  Arguments:
*     INLINE( * ) = INTEGER (Given)
*       Input line of data to be copied
*     DESLIN( * ) = INTEGER (Given)
*       Output line of copied data.
*     LIMIT = INTEGER (Given)
*       Limit of output data
*     OFFSET = INTEGER (Given)
*       Offset into input data line from which copying starts (-1).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-SEP-1991 (PDRAPER):
*        Original version - needed for dynamic arrays in PISAFIND.
*     17-JUN-1995 (PDRAPER):
*        Changed to work with INTEGER arrays.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER LIMIT
      INTEGER OFFSET
      INTEGER INLINE( * )

*  Arguments Returned:
      INTEGER DESLIN( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Copy data into output line.
      DO 1 I = 1, LIMIT
         DESLIN( I ) = INLINE( I + OFFSET )
 1    CONTINUE

      END
