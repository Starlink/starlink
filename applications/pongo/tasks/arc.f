      SUBROUTINE ARC( STATUS )
*+
*  Name:
*     ARC

*  Purpose:
*     Draw an arc of an ellipse.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ARC( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     A specified arc of an ellipse is drawn from the position angles of
*     the start and end of the arc, the semi axes, the position of the
*     centre and the rotation of the axes. If no parameters are
*     specified then whole ellipses can be drawn from the data stored in
*     the following data areas:
*
*        - XCOL -- X centre,
*        - YCOL -- Y centre,
*        - EXCOL -- semi-major axis,
*        - EYCOL -- semi-minor axis.

*  Usage:
*     arc [a] [b] [x0] [y0] [pastart] [paend] [rotation]

*  ADAM Parameters:
*     A = _REAL (Read and Write)
*        The semi-major axis of the ellipse.
*
*        If no value is specified on the command line, the current
*        value is used.  If there is no current value, a default value
*        1.0 is used.
*     B = _REAL (Read and Write)
*        The semi-minor axis of the ellipse.
*
*        If no value is specified on the command line, the current
*        value is used.  If there is no current value, a default value
*        1.0 is used.
*     X0 = _DOUBLE (Read and Write)
*        The X coordinate of the centre of the ellipse.
*
*        If no value is specified on the command line, the current
*        value is used. If there is no current value, a default value
*        0.0 is used.
*     Y0 = _DOUBLE (Read and Write)
*        The Y coordinate of the centre of the ellipse.
*
*        If no value is specified on the command line, the current
*        value is used. If there is no current value, a default value
*        0.0 is used.
*     PASTART = _REAL (Read and Write)
*        The position angle of the start of the arc (degrees).
*
*        If no value is specified on the command line, the current
*        value is used. If there is no current value, a default value
*        0.0 is used.
*     PAEND = _REAL (Read and Write)
*        The position angle of the end of the arc (degrees).
*
*        If no value is specified on the command line, the current
*        value is used. If there is no current value, a default value
*        360.0 is used.
*     ROTATION = _REAL (Read and Write)
*        The angle that the major axis makes with the horizontal
*        (degrees anti-clockwise).
*
*        If no value is specified on the command line, the current
*        value is used. If there is no current value, a default value
*        0.0 is used.
*     FROMDATA = _LOGICAL (Read)
*        If TRUE, the command will use the data already loaded to draw
*        whole ellipses, with positions and sizes specified as above.
*        [FALSE]
*     PROJECTION = _CHAR (Read)
*        The geometry that is to be used to plot the arc.  This is
*        explained in detail in the section on projections.  Allowed
*        values: "NONE", "TAN", "SIN", "ARC", "GLS", "AITOFF",
*        "MERCATOR" and "STG".
*
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
*        must be specified as dd:mm:ss.sss). This paramerter is only
*        required for PROJECTION values other than "NONE".
*
*        This parameter is not specified on the command line. The value
*        of the global parameter PONGO_DECCENTRE is used. If
*        PONGO_DECCENTRE is not defined, the default value "0" is used.
*     ERSCALE = _REAL (Read and Write)
*        The factor used to scale values in the EXCOL and EYCOL data
*        areas. This allows the ellipse axes lengths to be scaled,
*        changing the sizes of ellipses produced using the FROMFILE
*        parameter.
*
*        This parameter is not specified on the command line. The value
*        of the global parameter PONGO_ERSCALE is used. If
*        PONGO_ERSCALE is not defined, the default value 1.0 is used.

*  Examples:
*     ICL> ARC 1 1 0 0
*        Will draw a circle of radius 1 (world coordinates), assuming
*        the PASTART and PAEND parameters have their default values
*        (0.0 and 360.0 degrees respectively).
*     ICL> ARC FROMDATA
*        Draws arcs using the information read into the XCOL, YCOL,
*        EXCOL and EYCOL data areas.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     6-APR-1990 (JBVAD::PAH):
*        Original version.
*     19-OCT-1992 (PCTR):
*        Added contextual error report on exit.
*     21-JUN-1994 (PDRAPER):
*        Added check for device is open.
*     21-OCT-1994 (PDRAPER):
*        Added checks that the semi-major and minor axis have sane
*        values and an example showing how to draw arcs from data area
*        values.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PONGO_PAR'        ! PONGO global constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants

*  Global Variables:
      INCLUDE 'PONGO_CMN'        ! PONGO global variables

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL PON_DEVOP
      LOGICAL PON_DEVOP          ! PGPLOT device is open

*  Local Variables:
      LOGICAL FROMDATA           ! Switch indicating that the data are
                                 ! to be used for plotting

      INTEGER I                  ! Loop index
      INTEGER PROJECTION         ! Projection number

      REAL A                     ! Semi-major axis
      REAL B                     ! Semi-minor axis
      REAL PAEND                 ! Position angle of end of arc
      REAL PASTART               ! Position angle of start of arc
      REAL ROT                   ! Rotation of axes
      REAL SIZESCALE             ! Error scale factor

      DOUBLE PRECISION DEC0      ! Centre of the projection (radians)
      DOUBLE PRECISION RA0       ! Centre of the projection (radians)
      DOUBLE PRECISION X         ! X coordinate of centre
      DOUBLE PRECISION Y         ! Y coordinate of centre

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( PON_DEVOP( .TRUE., STATUS ) ) THEN
         CALL PAR_GET0L( 'FROMDATA', FROMDATA, STATUS )
         CALL PON_GETPROJ('PROJECTION', 'RACENTRE', 'DECCENTRE',
     :                    PROJECTION, RA0, DEC0, STATUS )
         CALL PAR_GET0R( 'ERSCALE', SIZESCALE, STATUS )

         IF ( FROMDATA ) THEN
            DO I = 1, NDAT
               CALL PON_ARCDRAW( PROJECTION, RA0, DEC0, 0.0, 360.0,
     :                           SIZESCALE*ERRX( I ),
     :                           SIZESCALE*ERRY( I ), XDATA( I ),
     :                           YDATA( I ), ZDATA( I ), STATUS )
            END DO
         ELSE
            CALL PAR_GET0R( 'PASTART', PASTART, STATUS )
            CALL PAR_GET0R( 'PAEND', PAEND, STATUS )

*  Get the semi-major and semi-minor values and make sure these are
*  sensible (positive non-zero).
            CALL PAR_GDR0R( 'A', 1.0, VAL__SMLR, VAL__MAXR, .FALSE., A,
     :                      STATUS )
            CALL PAR_GDR0R( 'B', 1.0, VAL__SMLR, VAL__MAXR, .FALSE., B,
     :                      STATUS )

            CALL PAR_GET0D( 'X0', X, STATUS )
            CALL PAR_GET0D( 'Y0', Y, STATUS )
            CALL PAR_GET0R( 'ROTATION', ROT, STATUS )
            CALL PON_ARCDRAW( PROJECTION, RA0, DEC0, PASTART, PAEND, A,
     :                        B, X, Y, ROT, STATUS )
         END IF
      END IF

*  Check the returned status and report a contextual error message if
*  necessary.
      IF ( STATUS .NE. SAI__OK ) CALL ERR_REP( 'ARC_END',
     :                              'ARC: Cannot draw an arc of ' //
     :                              'an ellipse.', STATUS )

      END
* $Id$
