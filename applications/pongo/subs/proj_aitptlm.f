      SUBROUTINE PROJ_AITPTLM( PHI0, THETA0, PHI, THETA, L, M, STATUS )
*+
*  Name:
*     PROJ_AITPTLM

*  Purpose:
*     Find direction cosines for position in the AIT projection

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PROJ_AITPTLM( PHI0, THETA0, PHI, THETA, L, M, STATUS )

*  Description:
*     Uses a Aitoff projection, which has an 'equal area' property
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
*     PHI = DOUBLE PRECISION (Given)
*        longitude/right ascension of point
*     THETA = DOUBLE PRECISION (Given)
*        latitude/declination of point
*     L = DOUBLE PRECISION (Returned)
*        direction cosine of displacement east
*     M = DOUBLE PRECISION (Returned)
*        direction cosine of displacement north
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

*  Arguments Given:
      DOUBLE PRECISION PHI0, THETA0, PHI, THETA

*  Arguments Returned:
      DOUBLE PRECISION L, M

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION GEOMPAR( 3 ) ! scaling parameters for the
                                 ! projection

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL PROJ_AITPAR( THETA0, GEOMPAR)
      CALL PROJ_AITPTLMQK(PHI0, THETA0, PHI, THETA, GEOMPAR, L, M,
     : STATUS)
      END
* $Id$
