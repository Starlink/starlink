      SUBROUTINE IRA_DRBRK( MAXBRK, OUT, BREAK, VBREAK, NBREAK, LENGTH,
     :                      STATUS )
*+
*  Name:
*     IRA_DRBRK

*  Purpose:
*     Return information about a plotted curve.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_DRBRK( MAXBRK, OUT, BREAK, VBREAK, NBREAK, LENGTH,
*                     STATUS )

*  Description:
*     This routine returns various items of information about the curve
*     most recently drawn using any of the routines IRA_DRGTC, IRA_DRMER
*     and IRA_DRPAR. If the graphics options LINES (see routine
*     IRA_DROPT) is set so that the drawing of curves is suppressed, the
*     information returned by this routine refers to the curve which
*     would have been drawn if LINES had been set to allow drawing of
*     curves.

*  Arguments:
*     MAXBRK = INTEGER (Given)
*        The size of the arguments VBREAK and BREAK. The symbolic
*        value IRA__MXBRK can be used. An error is reported if the
*        supplied arrays are too small.
*     OUT = LOGICAL (Returned)
*        If .true. then the curve was completely outside the plotting
*        zone, and so no part of it could be plotted.
*     BREAK( 2, MAXBRK ) = REAL (Returned)
*        Contains the world coordinates at which each break in the
*        plotted curve occurred. These breaks may be caused by
*        discontinuities in the transformation between sky coordinates
*        and image coordinates, or simply by the fact that the curve
*        went outside the plotting window. The start and end of the
*        curve are also considered to be "breaks" even when they occur
*        within the plotting window. The exception to this is if the
*        start and end are coincident. In this case no break is recorded
*        for either the start or the end. BREAK( 1, I ) holds the X
*        world coordinate value at the I'th break, and BREAK( 2, I )
*        holds the Y world coordinate value.
*     VBREAK( 2, MAXBRK ) = REAL (Returned)
*        Contains the unit vector (within the world coordinate system)
*        parallel to the tangent to the curve at each break. The sense
*        is such that the vector always points back along the plotted
*        section of the curve.
*     NBREAK = INTEGER (Returned)
*        The number of breaks for which information is returned in BREAK
*        and VBREAK.
*     LENGTH = REAL (Returned)
*        The plotted length of the curve (in world coordinates).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-NOV-1992 (DSB):
*        Original version.
*     {enter_further_changes_here}

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

*  Global Variables:
      INCLUDE 'IRA_COM'          ! IRA common blocks.
*        ACM_NBRK = INTEGER (Read)
*           The no. of breaks in the plotted curve.
*        ACM_OUT = LOGICAL (Read)
*           True if the curve was completely outside the plotting area.
*        ACM_BRK( 2, IRA__MXBRK ) = REAL (Read)
*           The world coords of each break in the curve.
*        ACM_VBRK( 2, IRA__MXBRK ) = REAL (Read)
*           The unit direction vector in world coords at of each break
*           in the curve.
*        ACM_LENG = REAL (Read)
*           The length of the plotted curve in world coordinates.

*  Arguments Given:
      INTEGER MAXBRK

*  Arguments Returned:
      LOGICAL OUT
      REAL BREAK( 2, MAXBRK )
      REAL VBREAK( 2, MAXBRK )
      INTEGER NBREAK
      REAL LENGTH

*  Status:
      INTEGER STATUS             ! Global status

*  External Routines:
      EXTERNAL IRA1_INIT         ! Initialise graphics options in common

*  Local Variables:
      INTEGER I                  ! Loop count.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Report an error if the arrays are too small.
      IF( MAXBRK .LT. ACM_NBRK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'A', ACM_NBRK )
         CALL MSG_SETI( 'B', MAXBRK )
         CALL ERR_REP( 'IRA_DRBRK_ERR1',
     :     'IRA_DRBRK: ^A breaks occurred; supplied arrays will only '//
     :     'hold ^B', STATUS )
         GO TO 999
      END IF

*  Check that a curve has been drawn. If not, report an error.
      IF( ACM_LENG .EQ. VAL__BADR ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'IRA_DRBRK_ERR2',
     :                 'IRA_DRBRK: No curve has yet been drawn.',
     :                 STATUS )
         GO TO 999
      END IF

*  Copy the values stored in common to the supplied arguments.
      OUT = ACM_OUT
      LENGTH = ACM_LENG
      NBREAK = ACM_NBRK

      DO I = 1, NBREAK
         BREAK( 1, I ) = ACM_BRK( 1, I )
         BREAK( 2, I ) = ACM_BRK( 2, I )
         VBREAK( 1, I ) = ACM_VBRK( 1, I )
         VBREAK( 2, I ) = ACM_VBRK( 2, I )
      END DO

*  If an error occurred, give a context message.
 999  CONTINUE

      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRA_DRBRK_ERR2',
     :  'IRA_DRBRK: Error returning information about a plotting line',
     :                 STATUS )

      END IF

      END
