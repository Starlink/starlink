      SUBROUTINE PROJ_MERLMPT( PHI0, THETA0, L, M, PHI, THETA, STATUS )
*+
*  Name:
*     MERLMPT

*  Purpose:
*     Find position for direction cosines in the MER projection

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PROJ_MERLMPT( PHI0, THETA0, L, M, PHI, THETA, STATUS )

*  Description:
*     Uses a MER projection
*
*     L is assumed to be positive to the east
*     M is assumed to be positive to the north
*
*     Based on the AIPS implementation of these geometries - see
*     AIPS memos 27 & 46 - Eric Greisen.
*

*  Arguments:
*     PHI0 = DOUBLE PRECISION (Given)
*        reference point longitude/right ascension
*     THETA0 = DOUBLE PRECISION (Given)
*        reference point latitude/declination
*     L = DOUBLE PRECISION (Given)
*        direction cosine of displacement east
*     M = DOUBLE PRECISION (Given)
*        direction cosine of displacement north
*     PHI = DOUBLE PRECISION (Returned)
*        longitude/right ascension of point
*     THETA = DOUBLE PRECISION (Returned)
*        latitude/declination of point
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-JAN-1990 (JBVAD::PAH):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ASTRO_PAR'        ! Standard astronomical parameters
      INCLUDE 'PROJ_PAR'         ! parameters for the proj routines

*  Arguments Given:
      DOUBLE PRECISION PHI0, THETA0, L, M

*  Arguments Returned:
      DOUBLE PRECISION PHI, THETA

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION MERPARS( 3 ) ! scaling parameters

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL PROJ_MERPAR(THETA0,MERPARS)
      CALL PROJ_MERLMPTQK( PHI0, THETA0, L, M, MERPARS, PHI, THETA,
     : STATUS )

      END


