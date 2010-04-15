
      SUBROUTINE IRA1_LLEX( IDA, SCS, XLO, YLO, XHI, YHI, GAPLON,
     :                      GAPLAT, ACEN, BCEN, IMLO, IMHI, IPLO, IPHI,
     :                      STATUS )
*+
*  Name:
*     IRA1_LLEX

*  Purpose:
*     Find the meridians and parallels covering an image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_LLEX( IDA, SCS, XLO, YLO, XHI, YHI, GAPLON, GAPLAT,
*                     ACEN, BCEN, IMLO, IMHI, IPLO, IPHI, STATUS )

*  Description:
*     This routine determines the placing of meridians and parallels to
*     cover an image. It obtains an estimate of the range of longitude
*     and latitude encompassed by an image by transforming a sparse
*     grid of points evenly spaced over the image from image to sky
*     coordinates. The gaps between meridians and parallels is then
*     chosen to give as close as possible to seven of each over the
*     range of longitude and latitude (unless explicit gaps have been
*     specified by the calling application, in which case those gaps
*     are used). Each meridian and parallel has an integer index. The
*     meridian with index zero is the one nearest to the mean longitude
*     found within the range, and the parallel with index zero is the
*     one nearest to the mean latitude. The range of indices needed to
*     cover the image is then estimated by doubling the range of
*     longitude and latitude found from the sparse grid.  This doubling
*     attempts to get round the problem that a sparse grid may not
*     sample the image sufficiently finely to discover the full ranges.
*     The resulting longitude range is limited to 360 degrees, and the
*     latitude limits are constrained to be within the range +90
*     degrees to -90 degrees.

*  Arguments:
*     IDA = INTEGER (Given)
*        The IRA identifier for the astrometry information.
*     SCS = character (Given)
*        The sky coordinate system. An abbreviation may be given, and a
*        default equinox is used if none is supplied.
*     XLO = REAL (Given)
*        The lower bound on the X image axis. Note, this is a single
*        precision value.
*     XHI = REAL (Given)
*        The upper bound on the X image axis. Note, this is a single
*        precision value.
*     YLO = REAL (Given)
*        The lower bound on the Y image axis. Note, this is a single
*        precision value.
*     YHI = REAL (Given)
*        The upper bound on the Y image axis. Note, this is a single
*        precision value.
*     GAPLON = DOUBLE PRECISION (Given and Returned)
*        The longitude gap between meridians, in radians. If the
*        supplied value is zero or negative, then a new value is
*        calculated and returned. Otherwise the supplied value is used.
*     GAPLAT = DOUBLE PRECISION (Given and Returned)
*        The latitude gap between parallels, in radians. If the
*        supplied value is zero or negative, then a new value is
*        calculated and returned. Otherwise the supplied value is used.
*     ACEN = DOUBLE PRECISION (Returned)
*        The longitude of the meridian with index zero.
*     BCEN = DOUBLE PRECISION (Returned)
*        The latitude of the parallel with index zero.
*     IMLO = INTEGER (Given)
*        The lowest meridian index needed to cover the image. Adjacent
*        meridians are separated by GAPLON in longitude.
*     IMHI = INTEGER (Given)
*        The highest meridian index needed to cover the image.
*     IPLO = INTEGER (Given)
*        The lowest parallel index needed to cover the image. Adjacent
*        parallels are separated by GAPLAT in latitude.
*     IPHI = INTEGER (Given)
*        The highest parallel index needed to cover the image.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-MAR-1992 (DSB):
*        Original version.
*     28-OCT-1994 (DSB):
*        Use succesively finer sparse grids until at least four grid points
*        fall inside the area covered by the projection.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants.
      INCLUDE 'IRA_PAR'          ! IRA_ constants.

*  Arguments Given:
      INTEGER IDA
      CHARACTER SCS*(*)
      REAL XLO
      REAL XHI
      REAL YLO
      REAL YHI

*  Arguments Returned:
      DOUBLE PRECISION GAPLON
      DOUBLE PRECISION GAPLAT
      DOUBLE PRECISION ACEN
      DOUBLE PRECISION BCEN
      INTEGER IMLO
      INTEGER IMHI
      INTEGER IPLO
      INTEGER IPHI

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      DOUBLE PRECISION EPS       ! Small angular increment.
      INTEGER NUDGAP             ! No. of "nice" micro-degree intervals.
      INTEGER NSAGAP             ! No. of "nice" intervals in seconds
                                 ! of arc.
      INTEGER NMERID             ! Nominal no. of meridians.
      INTEGER NPARAL             ! Nominal no. of parallels.
      INTEGER NSTGAP             ! No. of "nice" intervals in seconds
                                 ! of time.
      INTEGER SZGMAX             ! Max. size of the sparse grid

      PARAMETER ( EPS = 0.01*IRA__AS2R )
      PARAMETER ( NUDGAP = 31 )
      PARAMETER ( NSAGAP = 21 )
      PARAMETER ( NMERID = 7 )
      PARAMETER ( NPARAL = 7 )
      PARAMETER ( NSTGAP = 20 )
      PARAMETER ( SZGMAX = 4096 )

*  Local Variables:
      DOUBLE PRECISION A         ! Longitude value.
      DOUBLE PRECISION AA( SZGMAX )! Longitude in sparse grid.
      DOUBLE PRECISION AAMAX     ! Highest longitude in sparse grid.
      DOUBLE PRECISION AAMIN     ! Lowest longitude in sparse grid.
      DOUBLE PRECISION AHILIM    ! Highest longitude allowed in sparse
                                 ! grid.
      DOUBLE PRECISION ALOLIM    ! Lowest longitude allowed in sparse
                                 ! grid.
      DOUBLE PRECISION B         ! Latitude value.
      DOUBLE PRECISION BB( SZGMAX )! Latitude in sparse grid.
      DOUBLE PRECISION BBMAX     ! Highest latitude in sparse grid.
      DOUBLE PRECISION BBMIN     ! Lowest latitude in sparse grid.
      INTEGER          DMGRID    ! Dimension of sparse grid of points.
      REAL             DX        ! Gap in X between points in sparse
                                 ! grid.
      REAL             DY        ! Gap in Y between points in sparse
                                 ! grid.
      INTEGER          I         ! Loop count
      INTEGER          INDEX     ! Index of point within sparse grid.
      INTEGER          J         ! Loop count
      DOUBLE PRECISION LGPLAT    ! Gap in latitude between parallels.
      DOUBLE PRECISION LGPLON    ! Gap in longitude between meridians.
      INTEGER          NGOOD     ! No. of good grid points
      REAL             SAGAPS( NSAGAP)! Nice intervals in arc-seconds.
      REAL             STGAPS( NSTGAP)! Nice intervals in seconds of
                                 ! time.
      INTEGER          SZGRID    ! No. of points  in sparse grid.
      REAL             UDGAPS( NUDGAP)! Nice intervals in micro degrees.
      DOUBLE PRECISION XX( SZGMAX )! X values in sparse grid.
      DOUBLE PRECISION YY( SZGMAX )! Y values in sparse grid.
      REAL             Y         ! Image Y value (single precision).

*  Local Data:
      DATA STGAPS / 0.1, 0.2, 0.4, 0.5, 1.0, 2.0, 4.0, 5.0, 10.0, 20.0,
     :              30.0, 60.0, 120.0, 240.0, 300.0, 600.0, 1200.0,
     :              1800.0, 3600.0, 7200.0 /
      DATA SAGAPS / 1.0, 2.0, 3.0, 5.0, 10.0, 20.0, 30.0, 60.0, 120.0,
     :              240.0, 300.0, 600.0, 1200.0, 1800.0, 3600.0,
     :              7200.0, 14400.0, 18000.0, 36000.0, 72000.0,
     :              108000.0 /
      DATA UDGAPS / 1.0, 2.0, 4.0, 5.0, 10.0, 20.0, 40.0, 50.0, 100.0,
     :              200.0, 400.0, 500.0, 1000.0, 2000.0, 4000.0,
     :              5000.0, 10000.0, 20000.0, 40000.0, 50000.0,
     :              100000.0, 200000.0, 400000.0, 500000.0, 1.0E6,
     :              2.0E6, 4.0E6, 5.0E6, 10.0E6, 20.0E6, 30.0E6/

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the sparse grid dimensions.
      DMGRID = 4

*  Loop until a sparse grid with at least 4 good points in it has been
*  found.
      NGOOD = 0
      DO WHILE( NGOOD .LT. 4 .AND. STATUS .EQ. SAI__OK )

*  Double the current size of the sparse grid.
         DMGRID = 2*DMGRID

*  Set the total number of points in the sparse grid.
         SZGRID = DMGRID*DMGRID

*  If the maximum grid size has been exceeded, report an error and abort.
         IF( SZGRID .GT. SZGMAX .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'IRA1_LLEX_ERR1', 'The image area which'//
     :                    'contain valid sky co-ordinates is too '//
     :                    'small.', STATUS )
            GO TO 999
         END IF

*  Set up a grid of evenly spaced points in the image.
         DX = ( XHI - XLO )/REAL( DMGRID - 1 )
         DY = ( YHI - YLO )/REAL( DMGRID - 1 )
         INDEX = 0

         DO J = 1, DMGRID
            Y = YLO + ( J - 1 )*DY
            DO I = 1, DMGRID
               INDEX = INDEX + 1
               XX( INDEX ) = DBLE( XLO + ( I - 1 )*DX )
               YY( INDEX ) = DBLE( Y )
            END DO
         END DO

*  Transform these to sky coordinates.
         CALL IRA_TRANS( SZGRID, XX, YY, .TRUE., SCS, IDA, AA, BB,
     :                   STATUS )

*  Count the valid points.
         DO INDEX = 1, SZGRID
            IF( AA( INDEX ) .NE. VAL__BADD .AND.
     :          AA( INDEX ) .NE. VAL__BADD ) THEN
               A = AA( INDEX )
               B = BB( INDEX )
               NGOOD = NGOOD + 1
            END IF
         END DO

      END DO

*  Ensure that no point differs by more than 180 degrees in longitude
*  from the valid point just found. Also find the minimum and maximum
*  resulting longitude and latitude.
      ALOLIM = A - IRA__PI
      AHILIM = A + IRA__PI

      AAMAX = VAL__MIND
      AAMIN = VAL__MAXD
      BBMAX = VAL__MIND
      BBMIN = VAL__MAXD

      DO INDEX = 1, SZGRID
         A = AA( INDEX )
         B = BB( INDEX )
         IF( A .NE. VAL__BADD .AND. B .NE. VAL__BADD ) THEN

            IF( A .LT. ALOLIM ) THEN
               A = A + IRA__TWOPI
               AA( INDEX ) = A

            ELSE IF( A .GT. AHILIM ) THEN
               A = A - IRA__TWOPI
               AA( INDEX ) = A

            END IF

            AAMAX = MAX( AAMAX, A )
            AAMIN = MIN( AAMIN, A )
            BBMAX = MAX( BBMAX, B )
            BBMIN = MIN( BBMIN, B )

         END IF
      END DO

*  Store the central coordinates.
      ACEN = 0.5*( AAMAX + AAMIN )
      BCEN = 0.5*( BBMAX + BBMIN )

*  Find the longitude gap which gives the nominal number of meridians.
      LGPLON = ( AAMAX - AAMIN )/DBLE( NMERID - 1 )

*  Find the latitude gap which gives the nominal number of parallels.
      LGPLAT = ( BBMAX - BBMIN )/DBLE( NPARAL - 1 )

*  Get the nearest "nice" values.
      CALL IRA1_FNGP3( SCS, NSTGAP, STGAPS, NSAGAP, SAGAPS, NUDGAP,
     :                 UDGAPS, LGPLON, LGPLAT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Only use these gaps if the supplied gaps are zero or negative.
      IF( GAPLON .LE. 0.0D0 ) GAPLON = LGPLON
      IF( GAPLAT .LE. 0.0D0 ) GAPLAT = LGPLAT

*  Put the central point at the intersection of a meridian and parallel.
      ACEN = GAPLON*NINT( ACEN/GAPLON )
      BCEN = GAPLAT*NINT( BCEN/GAPLAT )

*  Double the latitude and longitude ranges to cover the possibility of
*  the full range not being found by the sparse grid used above. Limit
*  each range.
      AAMIN = ACEN - MIN( IRA__PI - EPS, 2.0D0*( ACEN - AAMIN ) )
      AAMAX = ACEN + MIN( IRA__PI - EPS, 2.0D0*( AAMAX - ACEN ) )
      BBMIN = MAX( -IRA__PIBY2 + EPS, 2.0D0*BBMIN - BCEN )
      BBMAX = MIN( IRA__PIBY2 - EPS, 2.0D0*BBMAX - BCEN )

*  The meridian with longitude of ACEN has an "index" of zero. Work out
*  the maxmimum and minimum meridian indices required to cover the
*  entire longitude range.
      IMLO = -INT( 1.0D-3 + ( ACEN - AAMIN )/GAPLON )
      IMHI = INT( 1.0D-3 + ( AAMAX - ACEN )/GAPLON )

*  Evaluate the maximum and minimum parallel indices required to cover
*  the entire latitude range.
      IPLO = -INT( 1.0D-3 + ( BCEN - BBMIN )/GAPLAT )
      IPHI = INT( 1.0D-3 + ( BBMAX - BCEN )/GAPLAT )

 999  CONTINUE

      END


