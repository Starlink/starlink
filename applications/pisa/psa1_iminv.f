      SUBROUTINE PSA1_IMINV( ARRIN, NPIX, ARROUT, IVAL, STATUS )
*+
*  Name:
*     PSA1_IMINV

*  Purpose:
*     Removes invalid pixels from an integer array.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     ARRIN( NPIX ) = INTEGER (Given)
*        Array of values.
*     NPIX = INTEGER (Given)
*        Number of values.
*     ARROUT( NPIX ) = INTEGER ( Returned )
*        Output array
*     IVAL = REAL (Returned)
*        Replacement value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-APR-1992 (PDRAPER):
*        Original version.
*     17-JUN-1995 (PDRAPER):
*        Converted to process INTEGER arrays.
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
      INTEGER ARRIN( NPIX )
      INTEGER IVAL

*  Arguments Returned:
      INTEGER ARROUT( NPIX )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      DO 1 I = 1, NPIX
         IF ( ARRIN( I ) .NE. VAL__BADI ) THEN
            ARROUT( I ) = ARRIN( I )
         ELSE
            ARROUT( I ) = IVAL
         END IF
 1    CONTINUE

      END
