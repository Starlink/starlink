      SUBROUTINE WORLD( STATUS )
*+
*  Name:
*     WORLD

*  Purpose:
*     Set the world coordinates for the plot.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Description:
*     Set up the world coordinate limits for the plot. The world
*     coordinate system is the one in which the data are plotted. It
*     is possible to specify the limits explicitly, or to have them
*     calculated from the range of the data that have been read, or to
*     recall the limits from a previous plot using AGI (SUN/48).

*  Usage:
*     world action
*        { [xmin] [xmax] [ymin] [ymax]
*        { piclab=?
*        action

*  ADAM Parameters:
*     ACTION = _CHAR (Read and Write)
*        The way in which the world coordinate limits are to be
*        determined:
*           - "DATA" -- The limits are calculated from the data limits,
*           with a small border added.
*           - "DATA0" -- The limits are calculated as for DATA, but the
*           origin is included as one of the data points.
*           - "GIVEN" -- The limits specified on the command line are
*           used.
*           - "RECALL" -- The coordinates are recalled from a previous
*           plot using the AGI database.
*
*        [The value is prompted for.]
*     XMIN = _REAL (Read and Write)
*        The world coordinate of the left-hand edge of the plot.
*
*        The application will determine the value if ACTION is one of
*        "DATA", "DATA0" or "RECALL". If ACTION="GIVEN" and no value is
*        specified on the command line, the value of the global
*        parameter PONGO_XMIN is used.
*     XMAX = _REAL (Read and Write)
*        The world coordinate of the right-hand edge of the plot.
*
*        The application will determine the value if ACTION is one of
*        "DATA", "DATA0" or "RECALL". If ACTION="GIVEN" and no value is
*        specified on the command line, the value of the global
*        parameter PONGO_XMAX is used.
*     YMIN = _REAL (Read and Write)
*        The world coordinate of the lower edge of the plot.
*
*        The application will determine the value if ACTION is one of
*        "DATA", "DATA0" or "RECALL". If ACTION="GIVEN" and no value is
*        specified on the command line, the value of the global
*        parameter PONGO_YMIN is used.
*     YMAX = _REAL (Read and Write)
*        The world coordinate of the upper edge of the plot.
*
*        The application will determine the value if ACTION is one of
*        "DATA", "DATA0" or "RECALL". If ACTION="GIVEN" and no value is
*        specified on the command line, the value of the global
*        parameter PONGO_YMAX is used.
*     PICLAB = _CHAR (Read and Write)
*        The AGI label of the picture to be recalled.
*
*        [If ACTION="RECALL", the value is prompted for.]
*     PROJECTION = _CHAR (Read and Write)
*        The geometry to be used for plotting the data.  This is
*        explained in more detail in the section on projections.
*        Allowed values: "NONE", "TAN", "SIN", "ARC", "GLS", "AITOFF",
*        "MERCATOR" and "STG".
*
*        [The value of the global parameter PONGO_PROJECTN is used. If
*        PONGO_PROJECTN is not defined, the default value "NONE" is
*        used.]
*     RACENTRE = _CHAR (Read and Write)
*        The centre of the projection in RA (i.e. the angle must be
*        specified as hh:mm:ss.sss). This parameter is only required for
*        PROJECTION values other than "NONE".
*
*        [The value of the global parameter PONGO_RACENTRE is used. If
*        PONGO_RACENTRE is not defined, the default value "0" is used.]
*     DECCENTRE = _CHAR (Read and Write)
*        The centre of the projection in declination (i.e. the angle
*        must be specified as dd:mm:ss.sss). This parameter is only
*        required for PROJECTION values other than "NONE".
*
*        [The value of the global parameter PONGO_DECCENTRE is used. If
*        PONGO_DECCENTRE is not defined, the default value "0" is used.]
*     ERSCALE = _REAL (Read)
*        The scale factor to be applied to the EXCOL and EYCOL data when
*        determining the limits of the world co-ordinates.  ERSCALE is
*        used only when ACTION = "DATA" or "DATA0".
*        [The value of the global parameter PONGO_ERSCALE is used. If
*        PONGO_ERSCALE is not defined, the default value 1.0 is used.]

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Implementation Deficiencies:
*     -  This application is not integrated well with AGI (clumsy at
*     the moment).

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     PDRAPER: P.W. Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     19-MAR-1990 (JBVAD::PAH):
*        Original version.
*     19-OCT-1992 (PCTR):
*        Added contextual error report on exit.
*     1994 May 4 (MJC):
*        In DATA mode where there is no projection, ensure that the
*        data limits are determined.
*     3-JUN-1994 (PDRAPER):
*        Removed unused variables.
*     6-JUN-1994 (PDRAPER):
*        Changed DCV_PAR to PRM_PAR.
*     20-JUN-1994 (PDRAPER):
*        Added check for device is open.
*     20-MAR-1995 (PDRAPER):
*        Now sets common block X and Y limits as well as globals.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PONGO_PAR'        ! PONGO global constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT global constants
      INCLUDE 'AGI_ERR'          ! AGI error codes
      INCLUDE 'AGI_PAR'          ! AGI global constants

*  Global Variables:
      INCLUDE 'PONGO_CMN'        ! PONGO global variables

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Used legth of string
      EXTERNAL PON_DEVOP
      LOGICAL PON_DEVOP          ! PGPLOT is open

*  Local Variables:
      CHARACTER * ( 20 ) ACTION  ! Action to be taken
      CHARACTER * ( 80 ) COMMENT ! AGI comment for current picture
      CHARACTER * ( AGI__SZLAB ) LABEL ! AGI label of current picture
      CHARACTER * ( AGI__SZLAB ) PICLAB ! Picture label to be found
      CHARACTER * ( AGI__SZNAM ) PICNAM ! AGI name for current picture

      LOGICAL FOUND              ! TRUE if label found

      INTEGER BASEID             ! Base picture ID
      INTEGER CURID              ! Current picture ID
      INTEGER IDAT               ! Counter
      INTEGER LENACT             ! Length of ACTION
      INTEGER PICID              ! Current picture ID
      INTEGER PROJECTION         ! Projection type
      INTEGER LSTAT              ! Local status flag

      REAL ERSCAL                ! The error scaling factor
      REAL RMAXTEMP              ! Temporary variable
      REAL RMINTEMP              ! Temporary variable
      REAL SSS                   ! Square root of the error scaling
      REAL XMINP                 ! World coodinate bounds
      REAL XMAXP                 ! World coodinate bounds
      REAL YMINP                 ! World coodinate bounds
      REAL YMAXP                 ! World coodinate bounds
      REAL XMINS                 ! Limit of bounds
      REAL XMAXS                 ! Limit of bounds
      REAL YMINS                 ! Limit of bounds
      REAL YMAXS                 ! Limit of bounds
      REAL XOFFSET               ! X-axis offset
      REAL YOFFSET               ! Y-axis offset

      DOUBLE PRECISION RA0       ! Projection centre
      DOUBLE PRECISION DEC0      ! Projection centre
      DOUBLE PRECISION L         ! Coordinates in projection plane
      DOUBLE PRECISION M         ! Coordinates in projection plane

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that PGPLOT is open
      IF ( .NOT. PON_DEVOP( .TRUE., STATUS ) ) GO TO 99

*  Get action required.
      CALL PAR_GET0C( 'ACTION', ACTION, STATUS )
      CALL PON_GETPROJ( 'PROJECTION', 'RACENTRE', 'DECCENTRE',
     :                  PROJECTION, RA0, DEC0, STATUS )

      LENACT = MAX( 1, CHR_LEN( ACTION ) )
      CALL CHR_UCASE( ACTION( : LENACT ) )

      IF ( STATUS .NE. SAI__OK ) GO TO 99
      IF ( ACTION( : LENACT ) .EQ. 'DATA' ) THEN

*     Set up initial limits.
         RMINTEMP = VAL__MAXR / 2
         RMAXTEMP = VAL__MINR / 2
      ELSE IF ( ACTION( : LENACT ) .EQ. 'DATA0' ) THEN

*     Otherwise ensure that the origin will be within the limits.
         RMINTEMP = 0.0
         RMAXTEMP = 0.0
      END IF

      IF ( ACTION( : 4 ) .EQ. 'DATA' ) THEN

         IF ( PROJECTION .EQ. 1 ) THEN

*  Obtain the scale factor.
            CALL PAR_GET0R( 'ERSCALE', ERSCAL, STATUS )

*  Get the scale factor to apply to the errors.  (Copied from READF.
*  Why is this done? --- MJC)
            SSS = SQRT( ABS( ERSCAL ) )

*  Determine the extreme data values.  First set the range to the upper
*  and lower floating-point values.
            XMIN = VAL__MAXR / 2.0
            XMAX = VAL__MINR / 2.0
            YMIN = VAL__MAXR / 2.0
            YMAX = VAL__MINR / 2.0
            XMINS = XMIN
            XMAXS = XMAX
            YMINS = YMIN
            YMAXS = YMAX

            DO IDAT = 1, NDAT
               XMAX = MAX( XMAX, REAL( XDATA( IDAT ) ) +
     :                     SSS * ERRX( IDAT ) )
               XMIN = MIN( XMIN, REAL( XDATA( IDAT ) ) -
     :                     SSS * ERRX( IDAT ) )
               YMAX = MAX( YMAX, REAL( YDATA( IDAT ) ) +
     :                     SSS * ERRY( IDAT ) )
               YMIN = MIN( YMIN, REAL( YDATA( IDAT ) ) -
     :                     SSS * ERRY( IDAT ) )
            END DO

*  Check that the values have moved from our extremes. If not set to
*  default values which shouldn't cause any problems in further
*  manipulations.
            IF ( XMIN .EQ. XMINS .AND. XMAX .EQ. XMAXS .AND.
     :           YMIN .EQ. YMINS .AND. YMAX .EQ. YMAXS ) THEN
               XMIN = -1
               XMAX = 1
               YMIN = -1
               YMAX = 1
            END IF

*  Allow for a border around the data values.
            XMINP = MIN( XMIN - ( XMAX - XMIN ) / 20.0, RMINTEMP )
            XMAXP = MAX( XMAX + ( XMAX - XMIN ) / 20.0, RMAXTEMP )
            YMINP = MIN( YMIN - ( YMAX - YMIN ) / 20.0, RMINTEMP )
            YMAXP = MAX( YMAX + ( YMAX - YMIN ) / 20.0, RMAXTEMP )
         ELSE
            XMINP = VAL__MAXR / 2.0
            XMAXP = VAL__MINR / 2.0
            YMINP = VAL__MAXR / 2.0
            YMAXP = VAL__MINR / 2.0
            XMINS = XMIN
            XMAXS = XMAX
            YMINS = YMIN
            YMAXS = YMAX

            DO IDAT = 1, NDAT
               LSTAT = SAI__OK
               CALL PROJ_CONVPTLM( PROJECTION-1, RA0, DEC0,
     :                             XDATA( IDAT ), YDATA( IDAT ),
     :                             L, M, LSTAT )

               IF ( LSTAT .EQ. SAI__OK ) THEN
                  XMINP =MIN( XMINP, REAL( L ) )
                  XMAXP =MAX( XMAXP, REAL( L ) )
                  YMINP =MIN( YMINP, REAL( M ) )
                  YMAXP =MAX( YMAXP, REAL( M ) )
               END IF
               LSTAT = SAI__OK
            END DO

*  Check that the values have moved from our extremes. If not set to
*  default values which shouldn't cause any problems in further
*  manipulations.
            IF ( XMINP .EQ. XMINS .AND. XMAXP .EQ. XMAXS .AND.
     :           YMINP .EQ. YMINS .AND. YMAXP .EQ. YMAXS ) THEN
               XMINP = -1
               XMAXP = 1
               YMINP = -1
               YMAXP = 1
            END IF

            XOFFSET = ( XMAXP - XMINP ) / 20.0
            XMINP = XMINP - XOFFSET
            XMAXP = XMAXP + XOFFSET
            YOFFSET = ( YMAXP - YMINP ) / 20.0
            YMINP = YMINP - YOFFSET
            YMAXP = YMAXP + YOFFSET
         END IF

         CALL PAR_DEF0R( 'XMIN', XMINP, STATUS )
         CALL PAR_DEF0R( 'XMAX', XMAXP, STATUS )
         CALL PAR_DEF0R( 'YMIN', YMINP, STATUS )
         CALL PAR_DEF0R( 'YMAX', YMAXP, STATUS )
      ELSE IF ( ACTION .EQ. 'RECALL' ) THEN

*  Need to find 'current' picture at the start of the search.
         CALL PAR_GET0C( 'PICLAB', PICLAB, STATUS )
         CALL CHR_UCASE( PICLAB )
         CALL CHR_LDBLK( PICLAB )

*  Find the base picture.
         CALL AGI_IBASE( BASEID, STATUS)
         CALL AGI_SELP( BASEID, STATUS )
         CALL AGI_RCL( ' ', PICID, STATUS )
         CALL AGI_ILAB( PICID, LABEL, STATUS )
         CALL CHR_UCASE( LABEL )
         CALL CHR_LDBLK( LABEL )
         FOUND = ( LABEL .EQ. PICLAB )

         DO WHILE ( ( .NOT. FOUND ) .AND. ( STATUS .EQ. SAI__OK ) )

*  Report the progress of the search.
            CALL MSG_SETC( 'LABEL', LABEL )
            CALL MSG_SETI( 'PICID', CURID )
            CALL MSG_OUT( ' ',
     :              'Picture ID = ^PICID , Label = ^LABEL', STATUS )
            CALL AGI_SELP( BASEID, STATUS )

*  Look for the next picture.
            CURID = PICID
            CALL AGI_RCP( ' ', CURID, PICID, STATUS )

*  Get the label and compare.
            CALL AGI_ILAB( PICID, LABEL, STATUS )
            CALL CHR_UCASE( LABEL )
            CALL CHR_LDBLK( LABEL )
            FOUND = ( LABEL .EQ. PICLAB )
         END DO

         IF ( FOUND .AND. STATUS .EQ. SAI__OK ) THEN
            CALL AGP_NVIEW( .FALSE., STATUS )
            CALL AGI_INAME( PICNAM, STATUS )
            CALL AGI_ICOM( COMMENT, STATUS )
            CALL MSG_SETC( 'PICNAM', PICNAM )
            CALL MSG_OUT( ' ',
     :              'Recalled picture is of type ^PICNAM.', STATUS )
            CALL MSG_SETC( 'COM', COMMENT( : CHR_LEN( COMMENT ) ) )
            CALL MSG_OUT( ' ', '--> ^COM', STATUS )
            CALL AGI_IWOCO( XMINP, XMAXP, YMINP, YMAXP, STATUS )
            CALL PAR_DEF0R( 'XMIN', XMINP, STATUS )
            CALL PAR_DEF0R( 'XMAX', XMAXP, STATUS )
            CALL PAR_DEF0R( 'YMIN', YMINP, STATUS )
            CALL PAR_DEF0R( 'YMAX', YMAXP, STATUS )
         END IF
      END IF

*  Get the values for the world coordinates. If using WORLD DATA etc.,
*  this relies on DYNAMIC as first part of VPATH, so that the values
*  just set as defaults above are now returned (if an explicit prompt
*  has been set, the dynamic defaults will be shown as the suggested
*  value).
      CALL PAR_GET0R( 'XMIN', XMINP, STATUS )
      CALL PAR_GET0R( 'XMAX', XMAXP, STATUS )
      CALL PAR_GET0R( 'YMIN', YMINP, STATUS )
      CALL PAR_GET0R( 'YMAX', YMAXP, STATUS )

      IF ( STATUS .NE. SAI__OK ) GO TO 99
      IF ( XMINP .EQ. XMAXP ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETR( 'XMIN', XMIN )
         CALL MSG_SETR( 'XMAX', XMAX )
         CALL ERR_REP( 'WORLD_XBAD', 'X-axis world coordinate ' //
     :                 'limits are bad (^XMIN to ^XMAX).', STATUS )
      ELSE IF ( YMINP .EQ. YMAXP ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETR( 'YMIN', YMIN )
         CALL MSG_SETR( 'YMAX', YMAX )
         CALL ERR_REP( 'WORLD_YBAD', 'Y-axis world coordinate ' //
     :                 'limits are bad (^YMIN to ^YMAX).', STATUS )
      END IF

      IF ( STATUS .EQ. SAI__OK ) THEN

*  Put the values of these parameters back (not needed?).
         CALL PAR_PUT0R( 'XMIN', XMINP, STATUS )
         CALL PAR_PUT0R( 'XMAX', XMAXP, STATUS )
         CALL PAR_PUT0R( 'YMIN', YMINP, STATUS )
         CALL PAR_PUT0R( 'YMAX', YMAXP, STATUS )

*  It seems to be that you need to put the dynamic values back, is
*  this an ADAM bug (or just because in monolith)?
         CALL PAR_DEF0R( 'XMIN', XMINP, STATUS )
         CALL PAR_DEF0R( 'XMAX', XMAXP, STATUS )
         CALL PAR_DEF0R( 'YMIN', YMINP, STATUS )
         CALL PAR_DEF0R( 'YMAX', YMAXP, STATUS )

*  Set up the window.
         CALL PGWINDOW( XMINP, XMAXP, YMINP, YMAXP )

*  And report the bounds of selected.
         CALL MSG_SETR( 'XMIN', XMINP )
         CALL MSG_SETR( 'XMAX', XMAXP )
         CALL MSG_SETR( 'YMIN', YMINP )
         CALL MSG_SETR( 'YMAX', YMAXP )
         CALL MSG_OUT( ' ',
     :        'World limits set to ^XMIN:^XMAX, ^YMIN:^YMAX', STATUS )

*  And set the common block bounds.
         XMIN = XMINP
         YMIN = YMINP
         XMAX = XMAXP
         YMAX = YMAXP
      ELSE

*  Error, so exit with sensible default bounds in common block.
         IF ( XMINP .EQ. XMAXP ) THEN
            XMIN = -1
            XMAX = 1
         ELSE
            XMIN = XMINP
            XMAX = XMAXP
         END IF
         IF ( YMINP .EQ. YMAXP ) THEN
            YMIN = -1
            YMAX = 1
         ELSE
            YMIN = YMINP
            YMAX = YMAXP
         END IF
      END IF

*  Check the returned status and report a contextual error message if
*  necessary.
 99   CONTINUE
      IF ( STATUS .NE. SAI__OK ) CALL ERR_REP( 'WORLD_END',
     :                              'WORLD: World coordinates for ' //
     :                              'the plot could not be set.',
     :                              STATUS )

      END
* $Id$
