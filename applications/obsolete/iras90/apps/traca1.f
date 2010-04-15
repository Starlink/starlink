      SUBROUTINE TRACA1( IDC, BSMP, ESMP, BDET, EDET, DETDAT, BAND,
     :                   REFRA, REFDEC, ADET, SCNDIR, INSCN, XSCN,
     :                   YMX, YMN, AVERAG, SGMA, NVAL, NAVAIL, STATUS )
*+
*  Name:
*     TRACA1

*  Purpose:
*     Get general information about a CRDD NDF file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TRACA1( IDC, BSMP, ESMP, BDET, EDET, DETDAT, BAND,
*                  REFRA, REFDEC, ADET, SCNDIR, INSCN, XSCN,
*                  YMX, YMN, AVERAGE, SGMA, NVAL, NAVAIL, STATUS )

*  Description:
*     This routine is used to get following information about a CRDD
*     NDF file:
*
*     1. The wave band number of the data contained in the file.
*
*     2. The RA and DEC of the reference position of the file.
*
*     3. The detector numbers of the detectors in the file..
*
*     4. Direction of the scan.
*
*     5. In-scan distance of each sample.
*
*     6. Cross-scan distance of each detector trace.

*  Arguments:
*     IDC = INTEGER (Given)
*        IRC identifier for the CRDD file.
*     BSMP = INTEGER (Given)
*        Index of the first sample in the data array of the CRDD file.
*     ESMP = INTEGER (Given)
*        Index of the last sample in the data array of the CRDD file.
*     BDET = INTEGER (Given)
*        The lowest detector Index in the data array of the CRDD file.
*     EDET = INTEGER (Given)
*        The highest detector Index in the data array of the CRDD file.
*     DETDAT( BSMP : ESMP, BDET : EDET ) = REAL (Given)
*        The input CRDD data.
*     BAND = INTEGER (Returned)
*        The wave band number of the data contained in the CRDD file.
*     REFRA = DOUBLE PRECISION (Returned)
*        The Right Ascension of the reference position in units of
*        radians (equinox B1950.0)
*     REFDEC = DOUBLE PRECISION (Returned)
*        The Declination of the reference position, in units of radians.
*        (equinox B1950.0)
*     ADET( I90__MAXDT ) = INTEGER (Returned)
*        The detector numbers of the detectors which contain more than
*        1 valid samples. The number of values written to this array is
*        returned in argument NAVAIL.
*     SCNDIR = LOGICAL (Returned)
*        The scan direction flag. It is true if the scan is from north
*        to south, otherwise it is false.
*     INSCN( BSMP : ESMP, BDET : EDET ) = REAL (Returned)
*        In-scan distance of each sample from the reference position,
*        positive if the displacement from the sample to the reference
*        position is in the same direction as the positive focal plane
*        Y axis.
*     XSCN( BDET : EDET ) = REAL (Returned)
*        Cross-scan offsets between the reference position and each
*        detector centre (at the position of closest approach of the
*        detector track to the reference point). Positive if the
*        displacement from the reference position to the detector track
*        is in the same direction as the positive focal plane Z axis.
*     YMX( BDET : EDET ) = REAL (Returned)
*        Max. value of each data trace.
*     YMN( BDET : EDET ) = REAL (Returned)
*        Min. value of each data trace.
*     AVERAG( BDET : EDET ) = REAL (Returned)
*        Average vaule of each data trace.
*     SGMA( BDET : EDET ) = REAL (Returned)
*        Sigma of each data trace.
*     NVAL( BDET : EDET ) = INTEGER (Returned)
*        The number of valid samples in each data trace.
*     NAVAIL = INTEGER (Returned)
*        The number of detectors which contains more than 1 valid
*        samples.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     2-APR-1991 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRA_PAR'          ! IRA_ package constants
      INCLUDE 'I90_DAT'          ! IRAS90 data.

*  Arguments Given:
      INTEGER IDC
      INTEGER BSMP
      INTEGER ESMP
      INTEGER BDET
      INTEGER EDET
      REAL DETDAT( BSMP : ESMP, BDET : EDET )

*  Arguments Returned:
      INTEGER BAND
      DOUBLE PRECISION REFRA
      DOUBLE PRECISION REFDEC
      INTEGER ADET( I90__MAXDT )
      LOGICAL SCNDIR
      REAL INSCN( BSMP : ESMP, BDET : EDET )
      REAL XSCN( BDET : EDET )
      REAL YMX( BDET : EDET )
      REAL YMN( BDET : EDET )
      REAL AVERAG( BDET : EDET )
      REAL SGMA( BDET : EDET )
      INTEGER NVAL( BDET : EDET )
      INTEGER NAVAIL

*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      INTEGER IRC_DETNO          ! Detector number of a detector
      REAL SLA_RANGE             ! put angle into [-pi, pi]

*  Local Variables:
      DOUBLE PRECISION ANGLE     ! Scan angle at a particular position
      DOUBLE PRECISION BETA      ! Ecliptic latitude.
      DOUBLE PRECISION DEC       ! DEC of a position
      DOUBLE PRECISION LAMBDA    ! Ecliptic longitude.
      DOUBLE PRECISION RA        ! DEC of a position

      INTEGER I                  ! Do loop indice
      INTEGER INDEX1             ! Detector index
      INTEGER INDEX2             ! Detector index
      INTEGER J                  ! Do loop indice
      INTEGER OBS                ! Observation number
      INTEGER SOP                ! The SOP number of the observation

      REAL CLIP( 1 )             ! Threshold used when calculate average
                                 ! of each data trace
      REAL CLSMP                 ! Closest sample to ref. position
      REAL INSCAN                ! In-scan distance in radians
      REAL NOMSPD                ! Nominal scan speed
      REAL SCNANG                ! Scan angle within [-pi, pi]
      REAL SPEED                 ! Scan speed at a particular position
      REAL XSCAN                 ! Cross-scan distance in radians

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get wave band number and reference position of the CRDD file.
      CALL IRC_INFO( IDC, BAND, REFRA, REFDEC, NOMSPD, SOP, OBS,
     :               STATUS )

*  Get the scan angle in equatorial coordinates at first sample of
*  first detector.
      CALL IRC_BPOS( IDC, 1, REAL( BSMP ), BDET, RA, DEC, ANGLE, SPEED,
     :               STATUS )

*  Convert the scan angle to ecliptic coordinates.
      CALL IRA_PACON( 1, RA, DEC, ANGLE, 'EQU', 'ECL', IRA__IRJEP,
     :                LAMBDA, BETA, ANGLE, STATUS )

*  Put scan angle into [-pi, pi].
      SCNANG = SLA_RANGE( REAL( ANGLE ) )

*  If scan angle is in [-pi/2, pi/2], the scan is from north to south.
      IF ( SCNANG .GT. -1.0*IRA__PIBY2 .AND.
     :     SCNANG .LT. IRA__PIBY2 ) THEN
          SCNDIR = .TRUE.

*  Otherwise, the scan is from south to north.
      ELSE
         SCNDIR = .FALSE.
      END IF

*  Get the statistical properties of each data trace.
      CLIP( 1 ) = 3.0
      CALL IRM_STATS( BSMP, ESMP, BDET, EDET, DETDAT, 1, CLIP,
     :                .FALSE., .TRUE., .FALSE., YMX, YMN, AVERAG,
     :                SGMA, NVAL, STATUS )

*  Obtain the information of each detector data trace.
      NAVAIL = 0
      DO J = BDET, EDET

*  If this trace contains more than 1 valid samples, this detector
*  is available for further processing. Note its detector number.
         IF ( NVAL( J ) .GT. 1 ) THEN
            NAVAIL = NAVAIL + 1
            ADET( NAVAIL ) = IRC_DETNO( IDC, J, STATUS )

*  Find the point on this detector track closest to the reference
*  position, and focal plane Z coordinate of the reference position
*  at the point of closest approach.
            CALL IRC_DCLAP( IDC, J, REFRA, REFDEC, CLSMP, XSCAN,
     :                      STATUS )

*  Get its cross-scan offset from the detector centre in arcmins.
            XSCN( J ) =  I90__DETZ( ADET( NAVAIL ) ) -
     :                   XSCAN * REAL( IRA__RTOD ) * 60.0

*  Get in-scan distance of each sample in this detector track.
            INDEX1 = J
            INDEX2 = J

            DO I = BSMP, ESMP
               CALL IRC_DIST( IDC, REAL( I ), INDEX1, CLSMP, INDEX2,
     :                        INSCAN, STATUS )

*  Convert to arcmins.
               INSCN( I, J ) = INSCAN * REAL( IRA__RTOD ) * 60.0

            END DO

         END IF

      END DO

      END
