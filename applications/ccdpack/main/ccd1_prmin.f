      SUBROUTINE CCD1_PRMIN( XIN, YIN, INDIN, NIN, DIFF, XOUT, YOUT,
     :                       INDOUT, NOUT, STATUS )
*+
*  Name:
*     CCD1_PRMIN

*  Purpose:
*     Rejects values which are closer than a certain distance.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_PRMIN( XIN, YIN, NIN, INDIN, DIFF, XOUT, YOUT, NOUT,
*                      INDOUT, STATUS )

*  Description:
*     This routines determines the euclidian distance between the all
*     the points XIN and YIN. Any points which are closer than the
*     given value DIFF are not copied to the output lists.

*  Arguments:
*     XIN( NIN ) = DOUBLE PRECISION (Given)
*        Input X positions.
*     YIN( NIN ) = DOUBLE PRECISION (Given)
*        Input Y positions.
*     INDIN( NIN ) = INTEGER (Given)
*        Identificiation numbers for the input points.
*     NIN = INTEGER (Given)
*        Number of positions to be checked for proximity.
*     DIFF = DOUBLE PRECISION (Given)
*        Minimum distance between input positions. Positions which are
*        closer than this value are rejected.
*     XOUT( NIN ) = DOUBLE PRECISION (Returned)
*        Output X positions.
*     YOUT( NIN ) = DOUBLE PRECISION (Returned)
*        Output Y positions.
*     INDOUT( NIN ) = INTEGER (Returned)
*        Identification numbers for the output points (by comparison
*        with the INDIN array you can tell where an input point has
*        ended up in the outputt list).
*     NOUT = INTEGER (Returned)
*        The number of positions output.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The INDIN/INDOUT arrays are supplied for use in re-ordering
*     any associated data.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 1997, 2001 Central Laboratory of the Research
*     Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-AUG-1992 (PDRAPER):
*        Original version.
*     24-SEP-1992 (PDRAPER):
*        Added indices option.
*     4-JUN-1997 (PDRAPER):
*        Added ability to deal with one input position.
*     19-FEB-2001 (MBT):
*        Added INDIN as well as INDOUT array.
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
      INTEGER INDIN( NIN )
      DOUBLE PRECISION DIFF

*  Arguments Returned:
      DOUBLE PRECISION XOUT( NIN )
      DOUBLE PRECISION YOUT( NIN )
      INTEGER INDOUT( NIN )
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
               INDOUT( NOUT ) = INDIN( I )
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
         INDOUT( NOUT ) = INDIN( I )
      END IF
      END
* $Id$
