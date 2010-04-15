      SUBROUTINE CDCRA4( NDF, IRC, RA, DEC, DETWID, MXCROS, NCROS,
     :                   CRSDTX, CRSDIS, CRSSMP, STATUS )
*+
*  Name:
*     CDCRA4

*  Purpose:
*     Find crossing detector traces of a CRDD NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CDCRA4( NDF, IRC, RA, DEC, DETWID, MXCROS, NCROS,
*                  CRSDTX, CRSDIS, CRSSMP, STATUS )

*  Description:
*     This subroutine is used to exam the detector traces of a CRDD NDF
*     to see whether they cross the given source position. The detector
*     index, the X-scan distance and the crossing sample number of
*     crossing detector traces are retuened.

*  Arguments:
*     NDF = INTEGER (Given)
*        NDF id of the CRDD NDF under consideration.
*     IRC = INTEGER (Given)
*        IRC id of the CRDD NDF under consideration.
*     RA, DEC = DOUBLE PRECISION (Given)
*        The sky coordinate of the specified source.
*     DETWID( I90__DETS ) = REAL (Given)
*        The width of each IRAS detectors.
*     MXCROS = INTEGER (Given)
*        The max. number of crossing for a given source.
*     NCROS = INTEGER (Given and Returned)
*        The number of crossing found so far.
*     CRSDTX( MXCROS ) = INTEGER (Given and Returned)
*        The index of the crossing detectors found so far.
*     CRSDIS( MXCROS ) = REAL (Given and Returned)
*        The X-scan distance of the crossing detectors found so far.
*     CRSSMP( MXCROS ) = REAL (Given and Returned)
*        The crossing sample of each crossing found so far.
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
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_DAT'          ! IRAS90 package constants
      INCLUDE 'IRA_PAR'          ! IRA_ package constants

*  Arguments Given:
      INTEGER NDF, IRC
      DOUBLE PRECISION RA, DEC
      REAL DETWID( I90__DETS )
      INTEGER MXCROS

*  Arguments Given and Returned:
      INTEGER NCROS
      INTEGER CRSDTX( MXCROS )
      REAL CRSDIS( MXCROS ), CRSSMP( MXCROS )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER IRC_DETNO          ! Detector number of a given index

*  Local Variables:
      REAL CLSMP                 ! Crossing sample of each detector
      REAL CLZFP                 ! Focal plan Z distance of the source
      INTEGER DET                ! Detector number
      INTEGER I                  ! Do loop index
      INTEGER LBND( 2 ), UBND( 2 ) ! NDF data-array bounds
      INTEGER NDIM               ! Number of dimension
      REAL XDIST                 ! X-scan distance of a detector trace

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the bounds of the NDF.
      CALL NDF_BOUND( NDF, 2, LBND, UBND, NDIM, STATUS )

*  Go through all detectors in the band to see which cross the expected
*  source.
      DO I = LBND( 2 ), UBND( 2 )
         DET = IRC_DETNO( IRC, I, STATUS )

*  Find the sample at which the detector is closest to the source and
*  the X-scan distance of the boresight at that moment.
         CALL IRC_DCLAP( IRC, I, RA, DEC, CLSMP, CLZFP, STATUS )

*  Get the X-scan distance of the detector at that moment.
         XDIST = CLZFP * REAL( IRA__RTOD ) * 60.0 - I90__DETZ( DET )

*  If this detector track cross the source, ...
         IF ( ABS( XDIST ) .LE. 0.5 * DETWID( DET ) ) THEN

*  Regard the it as a crossing detector if the crossing sample lies
*  inside the sample bounds.
            IF ( CLSMP .GT. LBND( 1 ) .AND. CLSMP .LT. UBND( 1 ) ) THEN
               NCROS = NCROS + 1

*  Note its crossing sample, crossing distance, file index in the group
*  and the detector index in the file.
               CRSDTX( NCROS ) = I
               CRSSMP( NCROS ) = CLSMP
               CRSDIS( NCROS ) = XDIST
            END IF
         END IF
      END DO

      END
