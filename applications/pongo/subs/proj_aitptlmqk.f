      SUBROUTINE PROJ_AITPTLMQK( PHI0, THETA0, PHI, THETA, GPAR,
     :                                              L, M, STATUS )
*+
*  Name:
*     PROJ_AITPTLM

*  Purpose:
*     Find direction cosines for position in the AIT projection

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PROJ_AITPTLMQK( PHI0, THETA0, PHI, THETA, GPAR, L, M, STATUS )

*  Description:
*     Uses a AIT projection
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
*     GPAR(3) = DOUBLE PRECISION (Given)
*        scaling parameters array containing f$_\alpha$,
*        f$_\delta$ and M$_0$
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
      INCLUDE 'PROJ_PAR'         ! PROJ library constants

*  Arguments Given:
      DOUBLE PRECISION PHI0, THETA0, PHI, THETA, GPAR(3)

*  Arguments Returned:
      DOUBLE PRECISION L, M

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION COST      ! cosine of theta
      DOUBLE PRECISION SINT      ! sine of theta
      DOUBLE PRECISION TEMP      ! temporary store
      DOUBLE PRECISION DPHI        ! PHI-PHI0

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Status will only be ok if all conditions fullfilled
      STATUS=PROJ__UNDEFINED
      COST=COS(THETA)
      SINT=SIN(THETA)
      DPHI=(PHI-PHI0)
      IF(DPHI.GT.DPI) DPHI=DPHI-D2PI
      IF(DPHI.LT.-DPI) DPHI=DPHI+D2PI

      DPHI=DPHI/2
      IF ( ABS(DPHI).LE.DPI .AND. ABS(THETA).LE.DPI/2 ) THEN
         TEMP=SQRT((1D0+COST*COS(DPHI))/2)
         IF ( ABS(TEMP).GT.PROJ__SMALL ) THEN
            L=2D0*GPAR(1)*COST*SIN(DPHI)/TEMP
            M=GPAR(2)*SINT/TEMP-GPAR(3)
            STATUS=SAI__OK
         END IF
      ELSE
         STATUS=PROJ__UNDEFINED
      END IF

      END
* $Id$
