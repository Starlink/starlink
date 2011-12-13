      SUBROUTINE CCD1_PNPLY( PX, PY, XX, YY, N, INOUT, STATUS )
*+
*  Name:
*     CCD1_PNPLY

*  Purpose:
*     Determine whether a point lies inside a polygon.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_PNPLY( PX, PY, XX, YY, N, INOUT, STATUS )

*  Description:
*     Given a point on a plane, and a list of vertices on that plane
*     representing a polygon, the routine determines whether
*     the point lies inside, outside, or on an edge/vertex of the
*     polygon.
*
*     The vertices may be listed clockwise or anticlockwise.
*     The first may optionally be repeated, if so N may optionally
*     be increased by 1.  The input polygon may be a compound polygon
*     consisting of several separate subpolygons. If so, the first
*     vertex of each subpolygon must be repeated, and when calculating
*     N, these first vertices must be counted twice.
*
*     The polygon may have at most MAXDIM (which currently equals 12)
*     vertices.  There is no limit to the number of vertices which the
*     algorithm can handle, so this limit can be increased if required.

*  Arguments:
*     PX = DOUBLE PRECISION (Given)
*        X coordinate of point whose inclusion is to be determined.
*     PY = DOUBLE PRECISION (Given)
*        Y coordinate of point whose inclusion is to be determined.
*     XX( N ) = DOUBLE PRECISION (Given)
*        X coordinates of polygon vertices.
*     YY( N ) = DOUBLE PRECISION (Given)
*        Y coordinates of polygon vertices.
*     N = INTEGER (Given)
*        Number of vertices in polygon.
*     INOUT = INTEGER (Returned)
*        Inclusion status of point at (PX, PY).  Possible return values
*        are:
*           -1  if the point is outside the polygon
*            0  if the point is on an edge or a vertex
*            1  if the point is inside the polygon.
*     STATUS = INTEGER (Given and returned)
*        The global status.

*  Algorithm:
*     A vertical line is drawn through the point in question.  If it
*     crosses the polygon an odd number of times, then the point is
*     inside of the polygon.
*
*     This algorithm, and most of the code itself, was taken from an
*     expanded entry from the comp.graphics.algorithms FAQ, written
*     by Randolph Franklin (University of Ottowa).

*  Copyright:
*     Copyright (C) 1970 Science & Engineering Research Council.
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     RF: Randolph Franklin (University of Ottowa)
*     MBT: Mark Taylor (STARLINK - IoA)
*     {enter_new_authors_here}

*  History:
*     1-JUL-1970 (RF):
*        Original version.
*     8-FEB-1999 (MBT):
*        Cosmetic changes for use in Starlink code.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER N
      DOUBLE PRECISION PX
      DOUBLE PRECISION PY
      DOUBLE PRECISION XX( N )
      DOUBLE PRECISION YY( N )

*  Arguments Returned:
      INTEGER INOUT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXDIM             ! Maximum number of points in polygon
      PARAMETER( MAXDIM = 12 )

*  Local Variables:
      INTEGER I
      INTEGER J
      LOGICAL MX
      LOGICAL MY
      LOGICAL NX
      LOGICAL NY
      DOUBLE PRECISION X( MAXDIM )
      DOUBLE PRECISION Y( MAXDIM )

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check we are within limits compiled into routine.
      IF ( N .GT. MAXDIM ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCD1_PNPLY',
     :      'Polygon has too many vertices to check inclusion of point',
     :      STATUS )
         GO TO 99
      END IF

*  Determine whether the point is included in the polygon.
      DO 1 I=1,N
         X( I ) = XX( I ) - PX
         Y( I ) = YY( I ) - PY
 1    CONTINUE
      INOUT = -1
      DO 2 I = 1, N
         J = 1 + MOD( I, N )
         MX = X( I ) .GE. 0D0
         NX = X( J ) .GE. 0D0
         MY = Y( I ) .GE. 0D0
         NY = Y( J ) .GE. 0D0
         IF ( .NOT. ( ( MY .OR. NY ) .AND. ( MX .OR. NX ) )
     :      .OR. ( MX .AND. NX ) ) GO TO 2
         IF ( .NOT. ( MY .AND. NY .AND. ( MX .OR. NX ) .AND. .NOT.
     :      (MX .AND .NX)) ) GO TO 3
         INOUT = -INOUT
         GO TO 2
 3       IF ( ( Y( I ) * X( J ) - X( I ) * Y( J ) ) /
     :      ( X( J ) - X( I ) ) ) 2, 4, 5
 4       INOUT = 0
         GO TO 99
 5       INOUT = -INOUT
 2    CONTINUE

*  Return
 99   CONTINUE

      END
* $Id$
