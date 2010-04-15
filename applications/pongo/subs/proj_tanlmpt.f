      SUBROUTINE PROJ_TANLMPT( PHI0, THETA0, L, M, PHI, THETA, STATUS )
*+
*  Name:
*     PROJ_TANLMPT

*  Purpose:
*     Find position for direction cosines in the TAN projection

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PROJ_TANLMPT( PHI0, THETA0, L, M, PHI, THETA, STATUS )

*  Description:
*     Uses a TAN projection
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
      DOUBLE PRECISION PHI0, THETA0, L, M

*  Arguments Returned:
      DOUBLE PRECISION PHI, THETA

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION COS0      ! cosine of theta
      DOUBLE PRECISION SIN0      ! sine of theta
      DOUBLE PRECISION PHIT      ! temporary store
      DOUBLE PRECISION THETAT    ! temporary store
      DOUBLE PRECISION TEMP      ! temporary store

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  only if all of the if conditions are satisfied will the answer be ok
      STATUS=PROJ__UNDEFINED

      COS0=COS(THETA0)
      SIN0=SIN(THETA0)
      TEMP=COS0-M*SIN0
      IF ( TEMP.NE.0D0 ) THEN
         PHIT=PHI0+ATAN2(L,TEMP)
         THETAT=ATAN(COS(PHIT-PHI0)*(M*COS0+SIN0)/TEMP)
         STATUS=SAI__OK
      END IF



*  make sure that PHI is in correct range
      IF ( STATUS .EQ. SAI__OK ) THEN
         PHI=PHIT
         THETA=THETAT
         IF ( PHI.GT.D2PI ) PHI=PHI-D2PI
         IF ( PHI.LT.0d0 ) PHI=PHI+D2PI
      END IF
      END
