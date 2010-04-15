      SUBROUTINE CDCRA7( NCRDD, IRCID, GVDTEG, SCNLEN, NCROS, CRSFLX,
     :                  CRSDTX, CRSDIS, DETWID, WTARY, STATUS )
*+
*  Name:
*     CDCRA7

*  Purpose:
*     Calculate distance effect in the coadding weights.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CDCRA7(  NCRDD, IRCID, GVDTEG, SCNLEN, NCROS, CRSFLX,
*                  CRSDTX, CRSDIS, DETWID, WTARY, STATUS )

*  Description:
*     This subroutine is used to calculate the distance effect on the
*     weights of the crossing traces to be coadded. The weights of each
*     crossing are further multiplied by a factor which is a Gaussian
*     function of the distance from the trace centre to the expected
*     source position. The width of the Gaussian function are determined
*     by the given value which specifies the value of the Gaussian
*     function at the edge of the detector.

*  Arguments:
*     NCRDD = INTEGER (Given)
*        Number of input CRDD NDF file.
*     IRCID( NCRDD ) = INTEGER (Given)
*        IRC id of the input CRDD NDFs
*     GVDTED = REAL (Given)
*        The value of the Gaussian function at the edge of the detector.
*     SCNLEN = INTEGER (Given)
*        The length of the crossing trace section to be extracted.
*     NCROS = INTEGER (Given)
*        Number of crossings.
*     CRSFLX( NCROS ) = INTEGER (Given)
*        File indices of the NDF the crossing traces belong to.
*     CRSDTX( NCROS ) = INTEGER (Given)
*        Detector indices of the crossing traces.
*     CRSDIS( NCROS ) = REAL (Given)
*        Crossing distance from the crossing trace centre to the
*        expected source position.
*     DETWID( I90__DETS ) = REAL (Given)
*        Width of the detectors.
*     WTARY( SCNLEN, NCROS ) = REAL (Given and Returned)
*        Weight of the crossing trace sections.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     1-DEC-1992 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Primitive constants
      INCLUDE 'I90_PAR'          ! IRAS90 package constants

*  Arguments Given:
      INTEGER NCRDD
      INTEGER IRCID( NCRDD )
      REAL GVDTEG
      INTEGER SCNLEN, NCROS
      INTEGER CRSFLX( NCROS ), CRSDTX( NCROS )
      REAL CRSDIS( NCROS )
      REAL DETWID( I90__DETS )

*  Arguments Given and Returned:
      REAL WTARY( SCNLEN, NCROS )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER IRC_DETNO          ! Det. No. from its index

*  Local Variables:
      INTEGER DET                ! Detector number
      REAL DISFCT                ! Distance factor
      REAL EDGDIS                ! Distance from det. edge to it centre
      REAL GFPAR                 ! Gaussian function parameter
      INTEGER I, J               ! Do loop indices

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Process all crossings one by one.
      DO I = 1, NCROS

*  Get its detector number.
         DET = IRC_DETNO( IRCID( CRSFLX( I ) ), CRSDTX( I ), STATUS )

*  Get the distance from the detector edge to its centre.
         EDGDIS = DETWID( DET ) / 2.0

*  If this distance is too small, something must be wrong when modifying
*  the detector width. Set status report and exit.
         IF ( EDGDIS .LE. VAL__SMLR ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'DET', DET )
            CALL ERR_REP( 'CDCRA7_ERR1', 'the width of detector ^DET '/
     :                   /'is too small, possibly an improer detector '/
     :                   /'width modification factor was selected.',
     :                     STATUS )
            GOTO 999
         END IF

*  Get the Gaussian function parameter such that the function has the
*  value GVDTEG at this distance.
         GFPAR = - LOG( GVDTEG ) / ( EDGDIS * EDGDIS )

*  Calculate the distance factor.
         DISFCT = EXP( - GFPAR * CRSDIS( I ) * CRSDIS( I ) )

*  Apply the factor to the weights this crossing trace as long as the
*  weight is valid original.
         DO J = 1, SCNLEN
            IF ( WTARY( J, I ) .NE. VAL__BADR )
     :         WTARY( J, I ) = DISFCT * WTARY( J, I )
         END DO
      END DO

 999  CONTINUE

      END
