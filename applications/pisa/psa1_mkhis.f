      SUBROUTINE PSA1_MKHIS( ARRAY, NPIX, IHIST, STATUS )
*+
*  Name:
*     PSA1_MKHIS

*  Purpose:
*     To form an INTEGER*2 histogram

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PSA1_MKHIS( ARRAY, NPIX, IHIST, STATUS )

*  Description:
*     The routine forms a histogram of the values in array ARRAY. The
*     values are integer*2 and are counted in the bin (from 101 to
*     32867) to which there value corresponds. The histogram is
*     returned in IHIST. THIS ROUTINE IS A PISAFIND CODE EXTRACT,
*     extracted for dynamic file allocation purposes.

*  Arguments:
*     ARRAY ( NPIX ) = INTEGER*2 (Given)
*        The array containing the data to histogram.
*     NPIX = INTEGER (Given)
*        The number of values in ARRAY.
*     IHIST( 32867 ) = INTEGER (Returned)
*        The histogram of values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-SEP-1991 (PDRAPER):
*        Original version - code chunk from PISAFIND (IMAGES).
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NPIX
      INTEGER * 2 ARRAY( NPIX )

*  Arguments Returned:
      INTEGER IHIST( 32867 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable
      INTEGER II                 ! Counter

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      DO 1 I = 1, NPIX
         II = ARRAY( I )
         II = II + 101
         IF ( II .GT. 32867 .OR. II .LT. 1 ) GO TO 1
         IHIST( II ) = IHIST( II ) + 1
 1    CONTINUE

      END
* $Id$
