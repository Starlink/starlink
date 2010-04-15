
      SUBROUTINE PSA1_RMINV( ARRIN, NPIX, ARROUT, RVAL, STATUS )
*+
*  Name:
*     PSA1_RMINV

*  Purpose:
*     Removes invalid pixels from a real array.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     ARRIN( NPIX ) = REAL (Given)
*        Array of values.
*     NPIX = INTEGER (Given)
*        Number of values.
*     ARROUT( NPIX ) = REAL( Returned )
*        Output array
*     RVAL = REAL (Returned)
*        Replacement value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-APR-1992 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants

*  Arguments Given:
      INTEGER NPIX
      REAL ARRIN( NPIX )
      REAL RVAL

*  Arguments Returned:
      REAL ARROUT( NPIX )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      DO 1 I = 1, NPIX
         IF ( ARRIN( I ) .NE. VAL__BADR ) THEN
            ARROUT( I ) = ARRIN( I )
         ELSE
            ARROUT( I ) = RVAL
         END IF
 1    CONTINUE

      END
* $Id$
