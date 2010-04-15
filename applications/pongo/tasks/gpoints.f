      SUBROUTINE GPOINTS( STATUS )
*+
*  Name:
*     GPOINTS

*  Purpose:
*     Plot points or lines between the data.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Description:
*     General plotting application. This application can be used simply
*     to plot a symbol at the position of each point, to plot a symbol
*     whose size depends upon the values in the ZCOL data area, or to
*     draw lines connecting the data points.

*  Usage:
*     gpoints action [symbol]

*  ADAM Parameters:
*     ACTION = _CHAR (Read)
*        The type of plot to produce. This can be "C", "P" or "S",
*        with the following meanings:
*
*        - "C" (connect) -- This action simply draws straight line
*        segments between the data points.
*
*        - "P" (points) -- Draw a symbol at each of the data points. The
*        symbol type that is used to mark each point is determined in
*        one of three ways:
*
*           o If no SYMBOL parameter is supplied on the command line,
*           and no symbol numbers have been read into the symbol data
*           area by READF, the point style will be set by the current
*           value of SYMBOL.
*
*           o If no SYMBOL parameter is supplied on the command line,
*           and values have been read into the symbol data area by
*           READF, the symbol number for each point will determine
*           the style of the point plotted.
*
*           o If SYMBOL is specified on the command line, it will
*           override each of the above options. The same specified
*           symbol will be used to mark all points.
*
*        The value of the symbol index should be an integer which
*        refers to the standard PGPLOT symbols.
*
*        - "S" (sizeplot) -- This action uses the values stored in the
*        ZCOL data area to determine the size of the plotted symbol.
*        The value of each entry in the ZCOL data area is effectively
*        used as an argument to a CHANGE CHEIGHT command before each
*        point is plotted. The SCALE parameter can be used to make
*        these values cover a reasonable range by multiplying the Z
*        data values.
*
*        [The value is prompted for.]
*     SYMBOL = _INTEGER (Read and Write)
*        The PGPLOT symbol number that is used to mark the data points.
*
*        If a value is specified on the command line, it will be used
*        for plotting symbols for all the data. If not value is
*        specified on the command line, the application attempts to use
*        the SYMCOL data for its symbols. If no symbol values have been
*        read into the SYMCOL data area, the current value is used for
*        all the data. The current value is initially set to 1.
*     SCALE = _REAL (Read and Write)
*        The scale factor used to multiply the ZCOL data values to get
*        a reasonable range of symbol sizes when ACTION="S".
*
*        If no value is specified on the command line, the current
*        value is used. The current value is initially set to 1.0.
*     PROJECTION = _CHAR (Read)
*        Specifies the geometry that is to be used to plot the data.
*        This is explained in more detail in the section on
*        projections.  Allowed values: "NONE", "TAN", "SIN", "ARC",
*        "GLS", "AITOFF", "MERCATOR" and "STG".
*
*        This parameter is not specified on the command line. The value
*        of the global parameter PONGO_PROJECTN is used. If
*        PONGO_PROJECTN is not defined, the default value "NONE" is
*        used.
*     RACENTRE = _CHAR (Read)
*        The centre of the projection in RA (i.e. the angle must be
*        specified as hh:mm:ss.sss). This parameter is only required for
*        PROJECTION values other than "NONE".
*
*        This parameter is not specified on the command line. The value
*        of the global parameter PONGO_RACENTRE is used. If
*        PONGO_RACENTRE is not defined, the default value "0" is used.
*     DECCENTRE = _CHAR (Read)
*        The centre of the projection in declination (i.e. the angle
*        must be specified as dd:mm:ss.sss). This parameter is only
*        required for PROJECTION values other than "NONE".
*
*        This parameter is not specified on the command line. The value
*        of the global parameter PONGO_DECCENTRE is used. If
*        PONGO_DECCENTRE is not defined, the default value "0" is used.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPER: P.W. Draper (STARLINK - Durham University )
*     {enter_new_authors_here}

*  History:
*     27-FEB-1990 (JBVAD::PAH):
*        Original version.
*     19-OCT-1992 (PCTR):
*        Add contextual error report on exit.
*     2-JUN-1994 (PDRAPER):
*       Removed unused variables.
*     21-JUN-1994 (PDRAPER):
*        Added check for device opened, removed illegal use of STATUS
*        variable.
*     8-MAY-1997 (PDRAPER):
*        Now accepts ! as no value for symbol (necessary under IRAF).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PONGO_PAR'        ! PONGO global constants
      INCLUDE 'PAR_ERR'          ! Parameter system error codes
      INCLUDE 'SUBPAR_PAR'       ! SUBPAR public global constants

*  Global Variables:
      INCLUDE 'PONGO_CMN'        ! PONGO global variables

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL PON_DEVOP
      LOGICAL PON_DEVOP          ! PGPLOT device is open

*  Local Variables:
      CHARACTER * ( 1 ) ACTION   ! Action

      INTEGER IDAT               ! Counter
      INTEGER ISYMTEMP           ! Temporary store of SYMBOL
      INTEGER PROJECTION         ! Projection type
      INTEGER SSTATE             ! State of the SYMBOL parameter
      INTEGER SYMBOL             ! PGPLOT symbol number
      INTEGER LSTAT              ! Local status

      REAL SCALE                 ! Scale factor for multiplication
      REAL TEMPH                 ! Current character height

      DOUBLE PRECISION DEC0      ! Projection centre
      DOUBLE PRECISION L         ! Projective coordinate
      DOUBLE PRECISION M         ! Projective coordinate
      DOUBLE PRECISION RA0       ! Projection centre

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that device is open.
      IF ( .NOT. PON_DEVOP( .TRUE., STATUS ) ) GO TO 99

*  Now get the parameters... If SYMBOL is null or hasn't been
*  set (on the command-line or in reponse to a forced prompt), then
*  record this as an suggestion to use the stored symbols.
      CALL PAR_GET0C( 'ACTION', ACTION, STATUS )
      CALL CHR_UCASE( ACTION )
      CALL PAR_STATE( 'SYMBOL', SSTATE, STATUS )
      CALL ERR_MARK
      CALL PAR_GET0I( 'SYMBOL', SYMBOL, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         SSTATE = SUBPAR__NULL
      END IF
      CALL ERR_RLSE

*  Get the projection parameters.
      CALL PON_GETPROJ( 'PROJECTION', 'RACENTRE', 'DECCENTRE',
     :                  PROJECTION, RA0, DEC0, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  For a point or SIZEPLOT option.
      IF ( ( ACTION.EQ.'P' )
     :     .OR. ( ACTION.EQ.'S' ) ) THEN
         IF ( ACTION.EQ. 'S' ) CALL PAR_GET0R( 'SCALE', SCALE, STATUS )
         CALL PGQCH( TEMPH )

         DO IDAT = 1, NDAT

*        Set scale factor if the points are to be scaled by the Z data.
            IF ( ACTION.EQ.'S' ) CALL PGSCH( ABS( ZDATA( IDAT )
     :                                            *SCALE ) )

*        If a symbol type was specified then use it.
            IF ( ( SYMCOL.EQ.0 )
     :           .OR. ( SSTATE.EQ.SUBPAR__ACTIVE ) ) THEN
               ISYMTEMP = SYMBOL
            ELSE

*           Else use the symbol read in from the files.
               ISYMTEMP = ISYMBS( IDAT )
            END IF

*        If no projection just go ahead and plot.
            IF ( PROJECTION.EQ.1 ) THEN
               CALL PGPOINT( 1, REAL( XDATA( IDAT ) ),
     :                       REAL( YDATA( IDAT ) ), ISYMTEMP )
            ELSE

*           Calculate the coordinates of the projection then plot.
               LSTAT = SAI__OK
               CALL PROJ_CONVPTLM( PROJECTION-1, RA0, DEC0,
     :                             XDATA( IDAT ), YDATA( IDAT ), L,
     :                             M, LSTAT )
               IF ( LSTAT.EQ.SAI__OK ) THEN
                  CALL PGPOINT( 1, REAL( L ), REAL( M ), ISYMTEMP )
               END IF
            END IF
         END DO

         CALL PGSCH( TEMPH )
      ELSE IF ( ACTION.EQ.'C' ) THEN

*     Connect the points with straight lines.
         IF ( PROJECTION.EQ.1 ) THEN
            CALL PON_CONVREAL( NDAT, XDATA, XDATATEMP )
            CALL PON_CONVREAL( NDAT, YDATA, YDATATEMP )
            CALL PGLINE( NDAT, XDATATEMP, YDATATEMP )
         ELSE
            STATUS=SAI__ERROR
            CALL ERR_REP( 'GPOINT_NOPLINE',
     :      'Polylines cannot be drawn on non-planar projections.',
     :      STATUS )
         END IF
      END IF

*  Check the returned status and report a contextual error message if
*  necessary.
 99   CONTINUE
      IF ( STATUS .NE. SAI__OK ) CALL ERR_REP( 'GPOINTS_END',
     :                              'GPOINTS: Unable to plot the data.',
     :                              STATUS )

      END
* $Id$
