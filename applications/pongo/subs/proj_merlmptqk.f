      SUBROUTINE PROJ_MERLMPTQK( PHI0, THETA0, L, M, GPAR, PHI,
     :       THETA, STATUS )
*+
*  Name:
*     PROJ_MERLMPTQK

*  Purpose:
*     Find position for direction cosines in the MER projection

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PROJ_MERLMPTQK( PHI0, THETA0, L, M, GPAR, PHI, THETA, STATUS )

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
*     GPAR(3) = DOUBLE PRECISION (GIVEN)
*        scaling parameters
*     PHI = DOUBLE PRECISION (Returned)
*        longitude/right ascension of point
*     THETA = DOUBLE PRECISION (Returned)
*        latitude/declination of point
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PDRAPER: P.W. Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     24-JAN-1990 (JBVAD::PAH):
*        Original version.
*     3-JUN-1994 (PDRAPER):
*        Removed unused variable.
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
      DOUBLE PRECISION PHI0, THETA0, L, M, GPAR(3)

*  Arguments Returned:
      DOUBLE PRECISION PHI, THETA

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION PHIT      ! temporary store
      DOUBLE PRECISION THETAT    ! temporary store
      DOUBLE PRECISION AMP       ! L*L+M*M
      DOUBLE PRECISION TEMP      ! temporary store

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      AMP=L*L+M*M

*  Only if all of the conditions are fulfilled will the projection be
*  valid
      STATUS=PROJ__UNDEFINED
      IF ( ABS(AMP).LE. D2PI*D2PI/2.5D0) THEN
         PHIT=L/GPAR(1)+PHI0
         TEMP=0.D0
         IF ( GPAR(2).NE.0D0 ) TEMP=(M+GPAR(3))/GPAR(2)
         TEMP=EXP(TEMP)
         THETAT=2D0*ATAN(TEMP)-D2PI/4D0
         STATUS=SAI__OK
      ELSE
         STATUS=PROJ__BADLM
      END IF



*  make sure that PHI is in correct range
      IF ( STATUS .EQ. SAI__OK ) THEN
         PHI=PHIT
         THETA=THETAT
         IF ( PHI.GT.D2PI ) PHI=PHI-D2PI
         IF ( PHI.LT.0D0 ) PHI=PHI+D2PI
      END IF
      END
