      SUBROUTINE GT_CIRCLE( STATUS )
*+
*  Name:
*     GT_CIRCLE

*  Purpose:
*     Draw a great circle between two points.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Description:
*     Draw a great circle between two points in current projection. The
*     great circle is specified by giving the coordinates in degrees of
*     two points on the celestial sphere. Either the small or large
*     great circle may be drawn.

*  Usage:
*     gt_circle [phistart] [thestart] [phiend] [theend]

*  ADAM Parameters:
*     PHISTART = _DOUBLE (Read and Write)
*        The longitude of the start of the great circle in degrees.
*
*        If no value is specified on the command line, the current
*        value is used. The current value is initially set to 0.0.
*     THESTART = _DOUBLE (Read and Write)
*        The latitude of the start of the great circle in degrees.
*
*        If no value is specified on the command line, the current
*        value is used. The current value is initially set to 0.0
*     PHIEND = _DOUBLE (Read and Write)
*        The longitude of the end of the great circle in degrees.
*
*        If no value is specified on the command line, the current
*        value is used. The current value is initially set to 0.0.
*     THEEND = _DOUBLE (Read and Write)
*        The latitude of the end of the great circle in degrees.
*
*        If no value is specified on the command line, the current
*        value is used. The current value is initially set to 0.0.
*     ACUTE = _LOGICAL (Read and Write)
*        If TRUE, the smaller great circle arc is drawn between the
*        given points.
*
*        If no value is specified on the command line, the current
*        value is used. The current value is initially set to TRUE.
*     PROJECTION = _CHAR (Read)
*        The geometry to be used to plot the great circle.  This is
*        explained in more detail in the section on projections.
*        Allowed values: "NONE", "TAN", "SIN", "ARC", "GLS", "AITOFF",
*        "MERCATOR" and "STG".
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
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     6-APR-1990 (JBVAD::PAH):
*        Original version.
*     19-OCT-1992 (PCTR):
*        Added contextual error report on exit.
*     20-JUN-1994 (PDRAPER):
*        Added check for device is open.
*     2-MAY-1997 (PDRAPER):
*        Added check and error message for valid projection.
*        Fixed so that end of circle is always reached.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PONGO_PAR'

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL PON_DEVOP         ! PGPLOT device is open
      LOGICAL PON_DEVOP

*  Local Variables:
      LOGICAL ACUTE              ! Determines whether the smaller or
                                 ! larger arc of the great circle is
                                 ! to be drawn

      INTEGER PROJECTION         ! Projection type

      DOUBLE PRECISION RA0       ! Projection centre
      DOUBLE PRECISION RA1       ! Great circle start
      DOUBLE PRECISION RA2       ! Great circle end
      DOUBLE PRECISION DEC0      ! Projection centre
      DOUBLE PRECISION DEC1      ! Great circle start
      DOUBLE PRECISION DEC2      ! Great circle end

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If device is open...
      IF ( PON_DEVOP( .TRUE., STATUS ) ) THEN
         CALL PON_GETPROJ( 'PROJECTION', 'RACENTRE', 'DECCENTRE',
     :                      PROJECTION, RA0, DEC0, STATUS )
         IF ( PROJECTION .EQ. 1 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'GT_CIRCLE_NONONE',
     :'This command can only be used with projections', STATUS )
         END IF
         IF ( STATUS .NE. SAI__OK ) GO TO 99


*  Get the coordinates of the points specifying the great circle.
         CALL PAR_GET0D( 'PHISTART', RA1, STATUS )
         CALL PAR_GET0D( 'THESTART', DEC1, STATUS )
         CALL PAR_GET0D( 'PHIEND', RA2, STATUS )
         CALL PAR_GET0D( 'THEEND', DEC2, STATUS )

*  If acute is true then the small great circle will be drawn.
         CALL PAR_GET0L( 'ACUTE', ACUTE, STATUS )

*  Make sure that the endpoints are in radians.
         RA1 = RA1 * DDEG2R
         RA2 = RA2 * DDEG2R
         DEC1 = DEC1 * DDEG2R
         DEC2 = DEC2 * DDEG2R

*  Draw the great circle.
         CALL GREAT_CIRCLE( PROJECTION-1, RA0, DEC0, RA1, DEC1, RA2,
     :                      DEC2, ACUTE, STATUS )
      END IF

*  Check the returned status and report a contextual error message if
*  necessary.
 99   CONTINUE
      IF ( STATUS .NE. SAI__OK ) CALL ERR_REP( 'GT_CIRCLE_END',
     :                              'GT_CIRCLE: Unable to draw the ' //
     :                              'great circle.', STATUS )

      END
* $Id$
