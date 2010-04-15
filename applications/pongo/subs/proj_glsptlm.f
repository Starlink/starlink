      SUBROUTINE PROJ_GLSPTLM( PHI0, THETA0, PHI, THETA, L, M, STATUS )
*+
*  Name:
*     PROJ_GLSPTLM

*  Purpose:
*     Find direction cosines for position in the GLS projection

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PROJ_GLSPTLM( PHI0, THETA0, PHI, THETA, L, M, STATUS )

*  Description:
*     Uses a GLS projection
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
      INCLUDE 'PROJ_PAR'        ! parameters for PROJ library

*  Arguments Given:
      DOUBLE PRECISION PHI0, THETA0, PHI, THETA

*  Arguments Returned:
      DOUBLE PRECISION L, M

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION DPHI        ! [local_variable_description]

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      DPHI=PHI-PHI0
      IF(DPHI.GT.DPI) DPHI=DPHI-D2PI
      IF(DPHI.LT.-DPI) DPHI=DPHI+D2PI
      IF ( ABS(DPHI).LE.D2PI .AND. ABS(THETA).LE.DPI/2 .AND.
     :                              ABS(THETA0).LE.DPI/2 ) THEN
         L=DPHI*COS(THETA)
         M=THETA-THETA0
      ELSE
         STATUS=PROJ__UNDEFINED
      END IF
      END
* $Id$
