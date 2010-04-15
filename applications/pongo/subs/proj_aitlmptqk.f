      SUBROUTINE PROJ_AITLMPTQK( PHI0, THETA0, L, M, GPAR, PHI,
     :                           THETA, STATUS )
*+
*  Name:
*     PROJ_AITLMPTQK

*  Purpose:
*     Find position for direction cosines in the AIT projection

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PROJ_AITLMPTQK( PHI0, THETA0, L, M, GPAR, PHI, THETA, STATUS )

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

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PDRAPER: P.W. Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     24-JAN-1990 (JBVAD::PAH):
*        Original version.
*     3-JUN-1994 (PDRAPER):
*        Removed unused variable SINT.
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
      DOUBLE PRECISION COST      ! cosine of theta
      DOUBLE PRECISION PHIT      ! temporary store
      DOUBLE PRECISION THETAT    ! temporary store
      DOUBLE PRECISION AMP       ! L*L+M*M
      DOUBLE PRECISION DZ        ! temporary store
      DOUBLE PRECISION TEMP      ! temporary store

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      AMP=L*L+M*M

*  Only if all of the conditions are fulfilled will the projection be
*  valid
      STATUS=PROJ__UNDEFINED
      IF ( ABS(AMP).LE. D2PI*D2PI/2.5D0) THEN
         IF ( L.EQ.0D0 .AND. M.EQ.0D0 ) THEN
            PHIT=0
            THETAT=0
            STATUS=SAI__OK
         ELSE
            DZ=4D0-L*L/(4D0*GPAR(1)*GPAR(1))-((M+GPAR(3))/GPAR(2))**2
            IF ( DZ.LE.4D0 .AND. DZ.GT.2 ) THEN
               DZ=SQRT(DZ)/2
               TEMP=(M+GPAR(3))*DZ/GPAR(2)
               IF ( ABS(TEMP).LE.1D0 ) THEN
                  THETAT=ASIN(TEMP)
                  COST=COS(THETAT)
                  IF ( COST.GT.PROJ__SMALL ) THEN
                     TEMP=L*DZ/GPAR(1)/COST/2D0
                     IF ( ABS(TEMP).LE.1D0 ) THEN
                        PHIT=PHI0+2D0*ASIN(TEMP)
                        STATUS= SAI__OK
                     END IF
                  END IF
               END IF
            END IF
         END IF
      ELSE
         STATUS=PROJ__BADLM
      END IF



*  make sure that PHI is in correct range
      IF ( STATUS .EQ. SAI__OK ) THEN
         PHI=PHIT
         THETA=THETAT
         IF ( PHI.GT.D2PI ) PHI=PHI-D2PI
         IF ( PHI.LT.0 ) PHI=PHI+D2PI
      END IF
      END
