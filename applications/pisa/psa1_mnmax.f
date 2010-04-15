      SUBROUTINE PSA1_MNMAX( ARRAY, NPIX, RMIN, RMAX, STATUS )
*+
*  Name:
*     PSA1_MNMAX

*  Purpose:
*     Finds the minimum and maximum value in ARRAY.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PSA1_MNMAX( ARRAY, NPIX, RMIN, RMAX, STATUS )


*  Arguments:
*     ARRAY( NPIX ) = REAL (Given)
*        Array of values.
*     NPIX = INTEGER (Given)
*        Number of values.
*     RMIN = REAL (Returned)
*        Minimum.
*     RMAX = REAL (Returned)
*        Maximum.
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
      REAL ARRAY( NPIX )

*  Arguments Returned:
      REAL RMIN
      REAL RMAX

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      RMIN = VAL__MAXR
      RMAX = VAL__MINR
      DO 1 I = 2, NPIX
         IF ( ARRAY( I ) .NE. VAL__BADR ) THEN
            RMIN = MIN( ARRAY( I ) , RMIN )
            RMAX = MAX( ARRAY( I ) , RMAX )
         END IF
 1    CONTINUE
      END
* $Id$
