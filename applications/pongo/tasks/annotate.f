      SUBROUTINE ANNOTATE( STATUS )
*+
*  Name:
*     ANNOTATE

*  Purpose:
*     Annotate the plotted data.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ANNOTATE( STATUS )

*  Description:
*     Each of the points on the plot is labeled with the appropriate
*     internal label (if it has been read from the data file). If no
*     parameters are specified, the default action is for the label to
*     be written with a zero offset in X and an offset of approximately
*     one character height in Y.

*  Usage:
*     annotate [xoff] [yoff] [justification]

*  ADAM Parameters:
*     XOFF = _REAL (Read and Write)
*        The X coordinate offset of the string relative to each data
*        point. The application will use the value 0.0 (i.e. no offset)
*        unless a value is given on the command line.
*        [0.0]
*     YOFF = _REAL (Read and Write)
*        The Y coordinate offset of the string relative to each data
*        point.
*
*        The application will prompt with a value of about 1/40th of
*        the height of the viewport unless a value is given on the
*        command line.
*
*        [1/40th of the viewport height.]
*     JUSTIFICATION = _REAL (Read and Write)
*        The justification about the point specified by XOFF and YOFF
*        relative to each data point (in the range 0.0 to 1.0).  Here,
*        0.0 means left justify the text relative to the data point,
*        1.0 means right justify the text relative to the data point,
*        0.5 means centre the string on the data point, other values
*        will give intermediate justifications.
*
*        If no value is specified on the command line, the current
*        value is used. The current value is initially set to 0.0.
*     PROJECTION = _CHAR (Read)
*        The projection that has been used to plot the data. This is
*        explained in more detail in the section on projections. Allowed
*        values: "NONE", "TAN", "SIN", "ARC", "GLS", "AITOFF",
*        "MERCATOR", and "STG".
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
*        Added contextual error message on exit.
*     20-JUN-1994 (PDRAPER):
*        Added check for device open.
*     16-AUG-1996 (PDRAPER):
*        Removed conversion of RA and DEC centres from arcseconds
*        to radians (this wasn't consistent with other similar
*        point plotting applications, i.e. GPOINT).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PONGO_PAR'        ! PONGO global constants

*  Global Variables:
      INCLUDE 'PONGO_CMN'        ! PONGO global variables

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL PON_DEVOP         ! PGPLOT device is open
      LOGICAL PON_DEVOP

*  Local Variables:
      DOUBLE PRECISION DEC0      ! Projection centre
      DOUBLE PRECISION L
      DOUBLE PRECISION M
      DOUBLE PRECISION RA0       ! Projection centre
      INTEGER IDAT               ! Counter
      INTEGER IFAIL              ! Status for the proj routines
      INTEGER PROJECTION         ! Projection type
      REAL JUST                  ! Justification for labels
      REAL SIZE                  ! Current setting of character height
      REAL XMAXT
      REAL XMINT
      REAL XOFF                  ! X offset for the labels
      REAL YMAXT
      REAL YMINT
      REAL YOFF                  ! Y offset for the labels
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that device is open.
      IF ( PON_DEVOP( .TRUE., STATUS ) ) THEN

*  Preliminary crude version - no user defined offsets.
         CALL PGQWIN( XMINT, XMAXT, YMINT, YMAXT )
         CALL PGQCH( SIZE )
         XOFF = 0.0
         YOFF = ( YMAXT - YMINT ) / ( 40.0 * SIZE )

*  Set up the default offsets.
         CALL PAR_DEF0R( 'XOFF', XOFF, STATUS )
         CALL PAR_DEF0R( 'YOFF', YOFF, STATUS )

*  Get the values.
         CALL PAR_GET0R( 'XOFF', XOFF, STATUS )
         CALL PAR_GET0R( 'YOFF', YOFF, STATUS )
         CALL PAR_GET0R( 'JUSTIFICATION', JUST, STATUS )

*  Get the projection parameters.
         CALL PON_GETPROJ( 'PROJECTION', 'RACENTRE', 'DECCENTRE',
     :                     PROJECTION, RA0, DEC0, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            DO IDAT = 1, NDAT
               IF ( PROJECTION .EQ. 1 ) THEN
                  CALL PGPTEXT( REAL( XDATA( IDAT ) )+XOFF,
     :                          REAL( YDATA( IDAT ) )+YOFF,
     :                          0.0, JUST, CLABELS( IDAT ) )
               ELSE
                  IFAIL = 0
                  CALL PROJ_CONVPTLM( PROJECTION-1, RA0, DEC0,
     :                                XDATA( IDAT ), YDATA( IDAT ),
     :                                L, M, IFAIL )

                  IF ( IFAIL .EQ. 0 ) CALL PGPTEXT( REAL( L )+XOFF,
     :                                              REAL( M )+YOFF,
     :                                              0.0, JUST,
     :                                              CLABELS( IDAT ) )
               END IF
            END DO
         END IF
      END IF

*  Check the returned status and report a contextual error message if
*  necessary.
      IF ( STATUS .NE. SAI__OK ) CALL ERR_REP( 'ANNOTATE_END',
     :                              'ANNOTATE: Cannot annotate the ' //
     :                              'plotted data.', STATUS )

      END
* $Id$
