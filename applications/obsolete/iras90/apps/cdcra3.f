      SUBROUTINE CDCRA3( PWGHT, PDIST, PGV, NCRDD, NDFID, WEIGHT, DIST,
     :                   GVDTEG, STATUS )
*+
*  Name:
*     CDCRA3

*  Purpose:
*     Get the way to weight the traces when coadding.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CDCRA3( PWGHT, PDIST, PGV, NCRDD, NDFID, WEIGHT, DIST,
*                  GVDTEG, STATUS )

*  Description:
*     This subroutine is used to get the weighting method to be used to
*     weight the traces when coadding. The basic weighting choices are:
*     Variance weighting (if all input CRDD NDFs contain variance
*     components ), NEFD weighting and Equal weighting. In addition,
*     these weighting methods can be combined with the distance
*     weighting which weights the detectors via a Gaussian function
*     according to the distance of the detector centre to the source
*     position (a detector acrossing the source at its centre will have
*     distance weighting 1.0 ). If distance weighting is selected, the
*     user will be prompted for the value of the Gaussian function at
*     the edge of the detector.

*  Arguments:
*     PWGHT = CHARACTER*( * ) (Given)
*        The name of the parameter used to get the method to weight the
*        traces to be coadded.
*     PDIST = CHARACTER*( * ) (Given)
*        The name of the parameter used to see if the distance weighting
*        will be combined with the other weighting methods.
*     PGV = CHARACTER*( * ) (Given)
*        The name of the parameter used when the distance weighting will
*        be used to get the value of the Guassian function at the edge
*        of a detector.
*     NCRDD = INTEGER (Given)
*        Number of input CRDD NDFs.
*     NDFID( NCRDD ) = INTEGER (Given)
*        NDF id of the input CRDD NDFs.
*     WEIGHT = CHARACTER*( * ) (Returned)
*        The weighting method selected by the user.
*     DIST = LOGICAL (Returned)
*        If true, the distance weighting will be combined with the other
*        weighting method.
*     GVDTEG = REAL (Returned)
*        The value of the Gaussian weighting function at the edge of a
*        detector when distance weighting is used.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     23-NOV-1992 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER*( * ) PWGHT, PDIST, PGV
      INTEGER NCRDD
      INTEGER NDFID( NCRDD )

*  Arguments Returned:
      CHARACTER*( * ) WEIGHT
      LOGICAL DIST
      REAL GVDTEG

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Do loop index
      LOGICAL VAR                ! All variance available flag
      LOGICAL VSTAT              ! Individual variance available flag
      CHARACTER*( 20 ) WGTOPT    ! Weighting option list
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if all input NDFs contain variance components.
      VAR = .TRUE.
      DO I = 1, NCRDD
         CALL NDF_STATE( NDFID( I ), 'Variance', VSTAT, STATUS )
         IF ( .NOT.VSTAT ) VAR = .FALSE.
      END DO

*  Compose the weighting option list according to availability of the
*  variance components of the input NDFs.
      IF ( VAR ) THEN
         WGTOPT = 'VARIANCE,NEFD,EQUAL'
      ELSE
         WGTOPT = 'NEFD,EQUAL'
      END IF

*  Get the weight method from the user.
      CALL PAR_CHOIC( PWGHT, 'NEFD', WGTOPT, .TRUE., WEIGHT, STATUS )

*  See if the distance weighting will be used.
      CALL PAR_GET0L( PDIST, DIST, STATUS )

*  If distance weighting will be used, get the value of the Guassian
*  function at the edge of a detector.
      IF ( DIST ) CALL PAR_GET0R( PGV, GVDTEG, STATUS )

      END
