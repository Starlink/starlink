      SUBROUTINE IRC_LIMIT( IDC, NDETS, DETIND, TRUNC, IDA, LBND, UBND,
     :                      STATUS )
*+
*  Name:
*     IRC_LIMIT

*  Purpose:
*     Determine the image area covered by a CRDD file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC_LIMIT( IDC, NDETS, DETIND, TRUNC, IDA, LBND, UBND,
*                     STATUS )

*  Description:
*     The positions of selected boundary samples within the supplied
*     CRDD file are converted to image coordinates using the supplied
*     IRA identifier (see ID2). If any of the samples fall outside the
*     image area defined by the supplied bounds, then the bounds are
*     updated so that the data will just fit in. The calling routine
*     supplies a list of the detectors which are to be included in the
*     image area.  The detector data streams in a CRDD file usually
*     start and finish at different in-scan positions, giving a
*     `ragged' end to a scan.  If the argument TRUNC is supplied true,
*     then a short section from each end of each data stream is
*     excluded from the process, so that all detector data streams
*     start and finish at the same in-scan position, resulting in a
*     `straight' end to the scan.

*  Arguments:
*     IDC = INTEGER (Given)
*        The IRC identifier for the CRDD file.
*     NDETS = INTEGER (Given)
*        The number of detector data streams to be included in the image
*        area.
*     DETIND( NDETS ) = INTEGER (Given)
*        A list of detector indices defining the detector data streams
*        to be included in the image area.
*     TRUNC = LOGICAL (Given)
*        True if the detector data streams are to be truncated so that
*        they all start and finish at the same in-scan position.  This
*        produces a `straight edge' across each end of the scan. If
*        TRUNC is false, then all data is included in the image area
*        from each detector data stream. This will in general result in
*        `ragged edges' across each end of the scan.
*     IDA = INTEGER (Given)
*        An IRA identifier for the astrometry information which is to be
*        used to define the projection from sky coordinates to image
*        coordinates. See ID2.
*     LBND( 2 ) = DOUBLE PRECISION (Given and Returned)
*        On entry, LBND holds the lower bounds on the two image
*        axes of the current image area (as pixel coordinates, not
*        indices). On exit, these values are updated if the given CRDD
*        file could not be fitted into the image area defined by the
*        values of LBND on entry.
*     UBND( 2 ) = DOUBLE PRECISION (Given and Returned)
*        On entry, UBND holds the upper bounds on the two image
*        axes of the current image area (as pixel coordinates, not
*        indices). On exit, these values are updated if the given CRDD
*        file could not be fitted into the image area defined by the
*        values of UBND on entry.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-NOV-1991 (DSB):
*        Orginal version.
*     21-JAN-1992 (DSB):
*        Modified to use IRC_TRUNC.
*     11-FEB-1992 (DSB):
*        Modified to use CCM_DETOR.
*     {original_version_entry}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants
      INCLUDE 'I90_DAT'          ! IRAS90 constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRC_PAR'          ! IRC constants.
      INCLUDE 'IRC_ERR'          ! IRC errors.

*  Global Variables:
      INCLUDE 'IRC_COM'          ! IRC common blocks.
*        CCM_VALID( IRC__MAX ) = LOGICAL (Read)
*           True if the corresponding IRC identifier is valid.
*        CCM_SLOW( IRC__MAX ) = INTEGER (Read)
*           Lowest sample number in the DATA array.
*        CCM_SHIGH( IRC__MAX ) = INTEGER (Read)
*           Highest sample number in the DATA array.
*        CCM_DLOW( IRC__MAX ) = INTEGER (Read)
*           Lowest detector index in the DATA array.
*        CCM_DHIGH( IRC__MAX ) = INTEGER (Read)
*           Highest detector index in the DATA array.
*        CCM_DETNO( IRC__MXD2S, IRC__MAX ) = INTEGER (Read)
*           The detector number corresponding to each detector index.
*        CCM_DETOR( IRC__MAX ) = INTEGER (Read)
*           The index within CCM_DETNO corresponding to row zero of the
*           NDF.

*  Arguments Given:
      INTEGER IDC
      INTEGER NDETS
      INTEGER DETIND( NDETS )
      LOGICAL TRUNC
      INTEGER IDA

*  Arguments Given and Returned:
      DOUBLE PRECISION LBND( 2 )
      DOUBLE PRECISION UBND( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION A( 18 + 2*I90__MAXDT )! Sky longitude values.
      DOUBLE PRECISION ANGLE( 18 + 2*I90__MAXDT )! Scan angle values.
      DOUBLE PRECISION B( 18 + 2*I90__MAXDT )! Sky latitude values.
      INTEGER   DET( 18 + 2*I90__MAXDT )! Detector indices of each point
                                 ! on the edges of the image area.
      INTEGER   DETNO            ! Detector number (1-62).
      INTEGER   EDGE1            ! Index of first edge detector index.
      INTEGER   EDGE2            ! Index of second edge detector index.
      REAL      FINISH( I90__MAXDT )! Finishing sample number for each
                                 ! detector.
      INTEGER   I                ! Loop count.
      DOUBLE PRECISION IX( 18 + 2*I90__MAXDT )! Image X coordinates.
      DOUBLE PRECISION IY( 18 + 2*I90__MAXDT )! Image Y coordinates.
      INTEGER   NPOS             ! No. of positions defining the edges
                                 ! of the image area covered by the CRDD
                                 ! file.
      REAL      SAM( 18 + 2*I90__MAXDT )! Sample numbers of each point on
                                 ! the edges of the image area.
      CHARACTER SCS*(IRA__SZSCS)! The Sky Coordinate System used by
                                 ! IRC1_DPOSI.
      REAL      SINC1            ! Sample increment between adjacent
                                 ! points on the first edge.
      REAL      SINC2            ! Sample increment between adjacent
                                 ! points on the second edge.
      REAL      SPEED( 18 + 2*I90__MAXDT )! Scan speed values.
      REAL      START( I90__MAXDT )! Starting sample number for each
                                 ! detector.
      REAL      Z                ! Cross-scan position.
      REAL      ZMAX             ! Maximum cross-scan position.
      REAL      ZMIN             ! Maximum cross-scan position.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the start and end of the scan section covered by all included
*  detectors.
      CALL IRC_TRUNC( IDC, NDETS, DETIND, START, FINISH, STATUS )

*  If the entire scan is to be included in the image area, set all the
*  starting and finishing samples equal.
      IF( .NOT. TRUNC ) THEN
         DO I = 1, NDETS
            START( I ) = REAL( CCM_SLOW( IDC ) )
            FINISH( I ) = REAL( CCM_SHIGH( IDC ) )
         END DO
      END IF

*  Find the indices corresponding to the edge detectors.
      ZMAX = VAL__MINR
      ZMIN = VAL__MAXR

      DO I = 1, NDETS

         DETNO = CCM_DETNO( DETIND( I ) + CCM_DETOR( IDC ), IDC )
         Z = I90__DETZ( DETNO )

         IF( Z .GT. ZMAX ) THEN
            ZMAX = Z
            EDGE1 = I
         END IF

         IF( Z .LT. ZMIN ) THEN
            ZMIN = Z
            EDGE2 = I
         END IF

      END DO

*  Set up lists of sample numbers and detector indices for which the
*  corresponding sky positions are required.  The list contains the two
*  ends of each data stream, and nine evenly spaced samples along each
*  of the edge detectors.
      NPOS = 1

      DO I = 1, NDETS
         SAM( NPOS ) = START( I )
         DET( NPOS ) = DETIND( I )
         SAM( NPOS + 1 ) = FINISH( I )
         DET( NPOS + 1 ) = DETIND( I )
         NPOS = NPOS + 2
      END DO

      SINC1 = ( FINISH( EDGE1 ) - START( EDGE1 ) + 1 )*0.1
      SINC2 = ( FINISH( EDGE2 ) - START( EDGE2 ) + 1 )*0.1

      DO I = 1, 9
         SAM( NPOS ) = START( EDGE1 ) + I*SINC1
         DET( NPOS ) = DETIND( EDGE1 )
         SAM( NPOS + 1 ) = START( EDGE2 ) + I*SINC2
         DET( NPOS + 1 ) = DETIND( EDGE2 )
         NPOS = NPOS + 2
      END DO

      NPOS = NPOS - 1

*  Find the sky coordinates corresponding to the list of samples set up
*  above.
      CALL IRC1_DPOSI( IDC, NPOS, SAM, DET, SCS, A, B, ANGLE, SPEED,
     :                 STATUS )

*  Transform these sky positions to image positions.
      CALL IRA_TRANS( NPOS, A, B, .FALSE., SCS, IDA, IX, IY, STATUS )

*  If all went OK ...
      IF( STATUS .EQ. SAI__OK ) THEN

         DO I = 1, NPOS
            LBND( 1 ) = MIN( LBND( 1 ), IX( I ) )
            UBND( 1 ) = MAX( UBND( 1 ), IX( I ) )
            LBND( 2 ) = MIN( LBND( 2 ), IY( I ) )
            UBND( 2 ) = MAX( UBND( 2 ), IY( I ) )
         END DO

      END IF

*  If an error occured, give a contextual message.
 999  CONTINUE

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRC_LIMIT_ERR1',
     :      'IRC_LIMIT: Unable to find image area covered by CRDD file',
     :                 STATUS )
      END IF

      END
