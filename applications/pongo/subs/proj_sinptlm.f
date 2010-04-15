      SUBROUTINE PROJ_SINPTLM( PHI0, THETA0, PHI, THETA, L, M, STATUS )
*+
*  Name:
*     PROJ_SINPTLM

*  Purpose:
*     Find direction cosines for position in the SIN projection

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PROJ_SINPTLM( PHI0, THETA0, PHI, THETA, L, M, STATUS )

*  Description:
*     Uses a SIN projection where the projected point is made by
*     drawing a line from the celestial sphere to the tangent plane of
*     the reference point, so that it meets the tangent plane at right
*     angles
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

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PDRAPER: P.W. Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     24-JAN-1990 (JBVAD::PAH):
*        Original version.
*     3-JUN-1994 (PDRAPER):
*        Removed unused variables.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ASTRO_PAR'        ! Standard astronomical parameters
      INCLUDE 'PROJ_PAR'

*  Arguments Given:
      DOUBLE PRECISION PHI0, THETA0, PHI, THETA

*  Arguments Returned:
      DOUBLE PRECISION L, M

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION COST,COS0      ! cosine of theta
      DOUBLE PRECISION SINT,SIN0      ! sine of theta
      DOUBLE PRECISION DPHI,TEMP
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      STATUS=PROJ__UNDEFINED
      DPHI=PHI-PHI0
      COST=COS(THETA)
      SINT=SIN(THETA)
      COS0=COS(THETA0)
      SIN0=SIN(THETA0)
      TEMP=SINT*SIN0+COST*COS0*COS(DPHI)
      IF(TEMP.GE.0) THEN
         L=COST*SIN(DPHI)
         M=SINT*COS0-COST*SIN0*COS(DPHI)
         STATUS=SAI__OK
      ENDIF

      END
* $Id$
