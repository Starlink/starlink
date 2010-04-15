      SUBROUTINE POSCA0( INDF, INDEX, RA, DEC, NCRDDF, NITEM, WORK,
     :                   STATUS )
*+
*  Name:
*     POSCA0

*  Purpose:
*     Store information about the supplied CRDD file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POSCA0( INDF, INDEX, RA, DEC, NCRDDF, NITEM, WORK, STATUS )

*  Description:
*     Information about the supplied CRDD file is stored in the work
*     array. Each row of the work array holds a different item of
*     information:
*
*     WORK( *, 1 ) = Index of CRDD file within the input group.
*     WORK( *, 2 ) = Minimum cross-scan distance from required position
*                    to the boresight track, in arc-mins.
*     WORK( *, 3 ) = In-scan distance between required position and the
*                    CRDD file reference position, in arc-mins.
*     WORK( *, 4 ) = Sample number at which the detector given by row 5
*                    reaches its closest point of approach to the
*                    required position.
*     WORK( *, 5 ) = The number of the detector which passes closest to
*                    the required position.
*     WORK( *, 6 ) = The position angle of the boresight track at the
*                    sample given by row 4.
*     WORK( *, 7 ) = SOP
*     WORK( *, 8 ) = OBS
*
*     If an error occurs, all values are returned equal to the bad
*     value VAL__BADD.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier for current CRDD file.
*     INDEX = INTEGER (Given)
*        Index of the current CRDD file within the input group.
*     RA = DOUBLE PRECISION (Given)
*        RA (B1950) of required position.
*     DEC = DOUBLE PRECISION (Given)
*        DEC (B1950) of required position.
*     NCRDDF = INTEGER (Given)
*        No. of NDFs in the input group.
*     NITEM = INTEGER (Given)
*        No. of items of information stored about each CRDD file.
*     WORK( NCRDDF, NITEM ) = DOUBLE PRECISION (Given and Returned)
*        The work array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-DEC-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants.
      INCLUDE 'I90_DAT'          ! IRAS90 data.
      INCLUDE 'IRA_PAR'          ! IRA_ constants.

*  Arguments Given:
      INTEGER INDF
      INTEGER INDEX
      DOUBLE PRECISION RA
      DOUBLE PRECISION DEC
      INTEGER NCRDDF
      INTEGER NITEM

*  Arguments Given and Returned:
      DOUBLE PRECISION WORK( NCRDDF, NITEM )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER IRC_DETNO          ! Detector number for a given index.

*  Local Variables:
      DOUBLE PRECISION A         ! DEC of a sample.
      DOUBLE PRECISION ANGLE     ! Scan angle at a sample.
      DOUBLE PRECISION B         ! RA of a sample.
      DOUBLE PRECISION REFDEC    ! DEC of reference point.
      DOUBLE PRECISION REFRA     ! RA of reference point.


      INTEGER BAND               ! Waveband index of CRDD file.
      INTEGER CLDETI             ! Index of detector which passes
                                 ! closest to the required position.
      INTEGER CLDETN             ! Number of detector which passes
                                 ! closest to the required position.
      INTEGER DETIND             ! Detector index.
      INTEGER DETNO              ! Detector number.
      INTEGER I                  ! Loop ccount.
      INTEGER IDC                ! IRC identifier for CRDD file.
      INTEGER LBND( 2 )          ! Lower bounds of CRDD file.
      INTEGER NDIM               ! No. of dimensions in CRDD file.
      INTEGER OBS                ! OBS number.
      INTEGER SOP                ! SOP number.
      INTEGER UBND( 2 )          ! Upper bounds of CRDD file.


      REAL CLSAMP                ! Sample number at position of closest
                                 ! approach.
      REAL CLZFP                 ! Focal plane Z coordinate at position
                                 ! of closest approach.
      REAL DIST                  ! In-scan distance between reference
                                 ! and required positions.
      REAL MINOFF                ! Minimum cross-scan offset from
                                 ! detector centre to required
                                 ! position.
      REAL NOMSPD                ! Nominal scan speed.
      REAL OFFSET                ! Cross-scan offset from detector
                                 ! centre to required position.
      REAL SAMP1                 ! Sample number at which the closest
                                 ! detector reaches its closest approach
                                 ! to the required position.
      REAL SAMP2                 ! Sample number at which the closest
                                 ! detector reaches its closest approach
                                 ! to the CRDD file reference position.
      REAL SPEED                 ! Actual scan speed at a sample.

*.

      CLDETN = 0

*  Store bad values to indicate that no valid data has yet been stored
*  for this NDF.
      DO I = 1, NITEM
         WORK( INDEX, I ) = VAL__BADD
      END DO

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store the NDF index as the first element in the workspace.
      WORK( INDEX, 1 ) = DBLE( INDEX )

*  Import the input CRDD file into the IRC system.
      CALL IRC_IMPRT( INDF, IDC, STATUS )

*  Get the bounds of the NDF.
      CALL NDF_BOUND( INDF, 2, LBND, UBND, NDIM, STATUS )

*  Find the sample number (for the detector with lowest index) at which
*  the boresight reaches its closest approach to the required position.
      CALL IRC_BCLAP( IDC, LBND( 2 ), RA, DEC, CLSAMP, CLZFP, STATUS )

*  Store the minimum cross scan offset in the workspace, in arc-mintes.
      WORK( INDEX, 2 ) = CLZFP*IRA__R2AM

*  Find the detector track which passes closest to the required
*  position.
      MINOFF = VAL__MAXR
      DO DETIND = LBND( 2 ), UBND( 2 )

         DETNO = IRC_DETNO( IDC, DETIND, STATUS )
         OFFSET = ABS( CLZFP*IRA__R2AM - I90__DETZ( DETNO ) )

         IF( OFFSET .LT. MINOFF ) THEN
            MINOFF = OFFSET
            CLDETI = DETIND
            CLDETN = DETNO
         END IF

      END DO

*  Find the sample number at which this detector passes closest to the
*  required position.
      CALL IRC_DCLAP( IDC, CLDETI, RA, DEC, SAMP1, CLZFP, STATUS )

*  If the required position crosses the detector mask, and the position
*  of closest approach is within the scan length, store the detector
*  and sample numbers in workspace. Otherwise, store zero for them
*  both.
      IF( MINOFF .LT. 0.5*I90__DETDZ( CLDETN ) .AND.
     :    NINT( SAMP1 ) .GE. LBND( 1 ) .AND.
     :    NINT( SAMP1 ) .LE. UBND( 1 ) ) THEN

         WORK( INDEX, 4 ) = DBLE( SAMP1 )
         WORK( INDEX, 5 ) = DBLE( CLDETN )

      ELSE
         WORK( INDEX, 4 ) = 0.0D0
         WORK( INDEX, 5 ) = 0.0D0

      END IF

*  Get the CRDD file reference position.
      CALL IRC_INFO( IDC, BAND, REFRA, REFDEC, NOMSPD, SOP, OBS,
     :               STATUS )

*  Store the SOP and OBS numbers.
      WORK( INDEX, 7 ) = DBLE( SOP )
      WORK( INDEX, 8 ) = DBLE( OBS )

*  Find the sample number at which this detector reaches its closest
*  approach to the CRDD file reference position.
      CALL IRC_DCLAP( IDC, CLDETI, REFRA, REFDEC, SAMP2, CLZFP, STATUS )

*  Find the in-scan distance between the reference point and the
*  required position.
      CALL IRC_DIST( IDC, SAMP1, CLDETI, SAMP2, CLDETI, DIST, STATUS )

*  Convert to arc-minutes and store in the workspace.
      WORK( INDEX, 3 ) = DIST*IRA__R2AM

*  Find the position angle of the focal plane Y axis at the point of
*  closest approach of the detector to the required position.
      CALL IRC_BPOS( IDC, 1, SAMP1, CLDETI, A, B, ANGLE, SPEED, STATUS )

*  Store it in the workspace (in degrees).
      WORK( INDEX, 6 ) = ANGLE*IRA__RTOD

*  Release the CRDD file from the IRC system.
      CALL IRC_ANNUL( IDC, STATUS )

*  If an error has occurred, store bad values to indicate that no valid
*  data has yet been stored for this NDF.
      IF( STATUS .NE. SAI__OK ) THEN
         DO I = 1, NITEM
            WORK( INDEX, I ) = VAL__BADD
         END DO
      END IF

      END
