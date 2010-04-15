      SUBROUTINE DRAWPOLY( STATUS )
*+
*  Name:
*     DRAWPOLY

*  Purpose:
*     Draw a polygon.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL DRAWPOLY( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine draws a polygon using the data in the XCOL and
*     YCOL areas to define the vertices of the region. The polygon
*     is shaded using the current fill-area style attributes (see
*     the FILLSTY command).

*  Usage:
*     drawpoly [fill]

*  ADAM Parameters:
*     FILL = _LOGICAL (Read)
*        Whether to fill the polygon or not. If TRUE then the current
*        fill-area style attributes as set by the CHANGE command will
*        be used.
*        [TRUE]
*     PROJECTION = _CHAR (Read)
*        The geometry to be used for plotting the polygon.  This is
*        explained in more detail in the section on projections.
*        Allowed values: "NONE", "TAN", "SIN", "ARC", "GLS", "AITOFF",
*        "MERCATOR" and "STG".
*
*        [The value of the global parameter PONGO_PROJECTN is used. If
*        PONGO_PROJECTN is not defined, the default value "NONE" is
*        used.]
*     RACENTRE = _CHAR (Read)
*        The centre of the projection in RA (i.e. the angle must be
*        specified as hh:mm:ss.sss). This parameter is only required for
*        PROJECTION values other than "NONE".
*
*        [The value of the global parameter PONGO_RACENTRE is used. If
*        PONGO_RACENTRE is not defined, the default value "0" is used.]
*     DECCENTRE = _CHAR (Read)
*        The centre of the projection in declination (i.e. the angle
*        must be specified as dd:mm:ss.sss). This parameter is only
*        required for PROJECTION values other than "NONE".
*
*        [The value of the global parameter PONGO_DECCENTRE is used. If
*        PONGO_DECCENTRE is not defined, the default value "0" is used.]

*  Notes:
*     - The XCOL and YCOL positions are assumed to be in radians.
*
*     - If a projection is used then expect the polygon to be incorrect
*       if it crosses any discontinuities, or if the edges are severely
*       distorted (the polygon is filled as plotted on the display
*       surface, not the sky).

*  Examples:
*     drawpoly
*        This draws a single polygon on the current graphics display. The
*        polygon is filled or shaded using the current fill-hatch styles.
*
*     drawpoly projection=aitoff deccentre="12:00:00"
*        This draws a single polygon on the current graphics display. The
*        polygon is filled or shaded using the current fill-hatch styles
*        and the polygon is drawn using an AITOFF projection. The lines
*        along the edges of the polygon are drawn as suitable arcs.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     30-APR-1997 (PDRAPER):
*        Original version.
*     1-OCT-2004 (TIMJ):
*        Use CNF_PVAL
*        Fix IPX, IPY memory leak
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'PONGO_PAR'       ! PONGO global constants
      INCLUDE 'PRM_PAR'         ! PRIMDAT global constants
      INCLUDE 'CNF_PAR'         ! For CNF_PVAL

*  Global Variables:
      INCLUDE 'PONGO_CMN'       ! PONGO global variables

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN           ! Used legth of string
      EXTERNAL PON_DEVOP
      LOGICAL PON_DEVOP         ! PGPLOT is open

*  Local Variables:
      DOUBLE PRECISION DEC0     ! Projection centre
      DOUBLE PRECISION RA0      ! Projection centre
      INTEGER IPX               ! Pointer to X workspace
      INTEGER IPY               ! Pointer to Y workspace
      INTEGER PROJECTION        ! Projection type
      REAL XD( NDATMAX )        ! Real X data
      REAL YD( NDATMAX )        ! Real Y data
      LOGICAL FILL              ! Whether to fill the polygon
      INTEGER FS                ! Fill area style

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that PGPLOT is open
      IF ( .NOT. PON_DEVOP( .TRUE., STATUS ) ) GO TO 99

*  Get the current fill-style immediately (this is reset on exit).
      CALL PGQFS( FS )

*  Check we have enough points.
      IF ( NDAT .LE. 2 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'DRAWPOLY_TOOFEW',
     :'There are not enough points to draw a polygon', STATUS )
         GO TO 99
      END IF

*  See if we want to fill the polygon.
      CALL PAR_GET0L( 'FILL', FILL, STATUS )
      IF ( .NOT. FILL ) CALL PGQFS( 2 )

*  Get the projection we need.
      CALL PON_GETPROJ( 'PROJECTION', 'RACENTRE', 'DECCENTRE',
     :                  PROJECTION, RA0, DEC0, STATUS )
      IF ( PROJECTION .EQ. 1 ) THEN

*  No projection, so just do things.
         CALL PON_CONVREAL( NDAT, XDATA, XD )
         CALL PON_CONVREAL( NDAT, YDATA, YD )
         CALL PGPOLY( NDAT, XD, YD )

      ELSE

*  Lines between vertices need to be drawn as arcs. These are restricted
*  to 100 points each.
         CALL PSX_CALLOC( NDAT * 100, '_REAL', IPX, STATUS )
         CALL PSX_CALLOC( NDAT * 100, '_REAL', IPY, STATUS )
         CALL PON_DPOLY( PROJECTION, RA0, DEC0, NDAT, XDATA, YDATA,
     :                   %VAL( CNF_PVAL ( IPX ) ),
     :                   %VAL( CNF_PVAL ( IPY ) ), STATUS )
         CALL PSX_FREE( IPX, STATUS )
         CALL PSX_FREE( IPY, STATUS )
      END IF

*  Report a contextual error if needed.
 99   CONTINUE

*  And restore the input fill style.
      CALL PGQFS( FS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'DRAWPOLY_END',
     :                 'DRAWPOLY: failed to draw polygon',
     :                 STATUS )
      END IF
      END
* $Id$
