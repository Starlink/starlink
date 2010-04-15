      SUBROUTINE PROJ_MERPTLMQK( PHI0, PHI, THETA, GPAR, L, M, STATUS )
*+
*  Name:
*     PROJ_MERPTLMQK

*  Purpose:
*     Find direction cosines for position in the MERCATOR projection

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PROJ_MERPTLMQK( PHI0, PHI, THETA, GPAR, L, M, STATUS )

*  Description:
*     Uses a MERCATOR projection
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
      INCLUDE 'PROJ_PAR'        ! PROJ parameters

*  Arguments Given:
      DOUBLE PRECISION PHI0, PHI, THETA, GPAR(3)

*  Arguments Returned:
      DOUBLE PRECISION L, M

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION DPHI        ! PHI-PHI0
      DOUBLE PRECISION TEMP      ! temporary store

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      DPHI=(PHI-PHI0)
      IF(DPHI.GT.DPI) DPHI=DPHI-D2PI
      IF(DPHI.LT.-DPI) DPHI=DPHI+D2PI
      IF ( ABS(DPHI).LE.DPI ) THEN
         L=GPAR(1)*DPHI
         TEMP=TAN(THETA/2+DPI/4)
         IF ( TEMP.GT.PROJ__SMALL ) THEN
            M=GPAR(2)*LOG(TEMP)-GPAR(3)
         ELSE
            STATUS=PROJ__UNDEFINED
         END IF
      ELSE
         STATUS=PROJ__UNDEFINED
      END IF

      END
* $Id$
