      SUBROUTINE CCD1_PRMIN( XIN, YIN, NIN, DIFF, XOUT, YOUT, INDEXS,
     :                       NOUT, STATUS )
*+
*  Name:
*     CCD1_PRMIN

*  Purpose:
*     Rejects values which are closer than a certain distance.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_PRMIN( XIN, YIN, NIN, DIFF, XOUT, YOUT, NOUT, INDEXS,
*                      STATUS )

*  Description:
*     This routines determines the euclidian distance between the all
*     the points XIN and YIN. Any points which are closer than the
*     given value DIFF are not copied to the output lists.

*  Arguments:
*     XIN( NIN ) = DOUBLE PRECISION (Given)
*        Input X positions.
*     YIN( NIN ) = DOUBLE PRECISION (Given)
*        Input Y positions.
*     NIN = INTEGER (Given)
*        Number of positions to be checked for proximity.
*     DIFF = DOUBLE PRECISION (Given)
*        Minimum distance between input positions. Positions which are
*        closer than this value are rejected.
*     XOUT( NIN ) = DOUBLE PRECISION (Returned)
*        Output X positions.
*     YOUT( NIN ) = DOUBLE PRECISION (Returned)
*        Output Y positions.
*     INDEXS( NIN ) = INTEGER (Returned)
*        Array of indices which record the original positions of the
*        output data within the input data array.
*     NOUT = INTEGER (Returned)
*        The number of positions output.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The indexs array is supplied for use in re-ordering any
*     associated data.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-AUG-1992 (PDRAPER):
*        Original version.
*     24-SEP-1992 (PDRAPER):
*        Added indices option.
*     4-JUN-1997 (PDRAPER):
*        Added ability to deal with one input position.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants

*  Arguments Given:
      INTEGER NIN
      DOUBLE PRECISION XIN( NIN )
      DOUBLE PRECISION YIN( NIN )
      DOUBLE PRECISION DIFF

*  Arguments Returned:
      DOUBLE PRECISION XOUT( NIN )
      DOUBLE PRECISION YOUT( NIN )
      INTEGER INDEXS( NIN )
      INTEGER NOUT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION XDIFF
      DOUBLE PRECISION YDIFF
      DOUBLE PRECISION RADIUS
      DOUBLE PRECISION DIFFSQ
      INTEGER I
      INTEGER J
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Square input distance.
      DIFFSQ = DIFF * DIFF

*  Initialise number of output values.
      NOUT = 0

*  If only have one point then just return it.
      IF ( NIN .EQ. 1 ) THEN 
         NOUT = -1
         I = 1
      ELSE

*  Check every combination of positions.
         DO 1 I = 1, NIN - 1
            RADIUS = VAL__MAXD
            DO 2 J = I + 1, NIN

*  Determine the minimum radius squared of point I from all the other
*  points.
               XDIFF = XIN( I ) - XIN( J )
               YDIFF = YIN( I ) - YIN( J )
               RADIUS = MIN( RADIUS, XDIFF * XDIFF + YDIFF * YDIFF )
 2          CONTINUE

*  If the minimum radius squared is less than the minimum given radius
*  squared then reject this point. Otherwise copy it to the output
*  arrays.
            IF ( RADIUS .GT. DIFFSQ ) THEN

*  Point accepted copy to the output list.
               NOUT = NOUT + 1
               XOUT( NOUT ) = XIN( I )
               YOUT( NOUT ) = YIN( I )
               
*  Record its original position.
               INDEXS( NOUT ) = I
            END IF
 1       CONTINUE
      END IF

*  Check that some values have remained.
      IF ( NOUT .EQ. 0 ) THEN

*  All rejected report error.
         STATUS = SAI__ERROR
         CALL MSG_SETD( 'DIFF', DIFF )
         CALL ERR_REP( 'CCD1_PRMIN',
     :   '  All points rejected. All are within radius ^DIFF of '//
     :   'each other', STATUS )
      ELSE

*  Final position needs copying - this should be ok as all points close
*  to it are rejected by now (also traps one input position).
         NOUT = MAX( 1, NOUT + 1 )
         XOUT( NOUT ) = XIN( NIN )
         YOUT( NOUT ) = YIN( NIN )

*  Record its original position.
         INDEXS( NOUT ) = I
      END IF
      END
* $Id$
