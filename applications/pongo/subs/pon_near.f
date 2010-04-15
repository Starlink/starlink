      INTEGER FUNCTION PON_NEAR( XYDATA, NDAT, X, Y, XDIST, YDIST )
*+
*  Name:
*     PON_NEAR

*  Purpose:
*     Calculate which data point is nearest the cursor position.

*  Language:
*     FORTRAN 77

*  Invocation:
*     RESULT = PON_NEAR( XYDATA, NDAT, X, Y, XDIST, YDIST )

*  Description:
*     Given an (X,Y) position in world coordinates (such as read from a
*     cursor position), this routine will hunt through the data to find
*     the data point which is closest to that position. The relative
*     ranges of the world coordinates of the axes are taken into
*     account so that the point chosen is the one that is nearest on
*     the screen.
*
*     The function returns the index of the closest data point, the
*     distance from that point is returned in XDIST and YDIST.

*  Arguments:
*     XYDATA( NDAT*2 ) = REAL (Given)
*        The values of the X and Y coordinates of the data points.
*        The X coordinate occupy the first NDAT positions and the Y
*        coordinates the last NDAT+1 to NDAT*2
*     NDAT = INTEGER (Given)
*        The number of X and Y data points.
*     X = REAL (Given)
*        The X coordinate of the cursor.
*     Y = REAL (Given)
*        The Y coordinate of the cursor.
*     XDIST = REAL (Returned)
*        The X distance to the selected point.
*     YDIST = REAL (Returned)
*        The Y distance to the selected point.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Returned Value:
*     PON_NEAR = INTEGER
*        The index of the nearest data point.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPER: P.W. Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     10-APR-1990 (JBVAD::PAH):
*        Original version.
*     24-JUN-1992 (PCTR):
*        Code tidy and prologue changes.
*     3-JUN-1994 (PDRAPER):
*        Merged the X and Y positions into one array. This was being
*        called with the same array indexed to give the X and Y positions
*        in separate arrays.
*     6-JUN-1994 (PDRAPER):
*        Removed unnecessary include file (DCV_PAR).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER NDAT

      REAL XYDATA( * )
      REAL X
      REAL Y

*  Arguments Returned:
      REAL XDIST
      REAL YDIST

*  Local Variables:
      REAL DIST                  ! Distance to point
      REAL XMAXP                 ! New window settings
      REAL XMINP                 ! New window settings
      REAL XTEMP                 ! Temporary X value
      REAL YMAXP                 ! New window settings
      REAL YMINP                 ! New window settings
      REAL YTEMP                 ! Temporary Y value

      INTEGER IX                 ! Counter
      INTEGER IY                 ! Counter

*.

*  Find the world coordinates.
      CALL PGQWIN( XMINP, XMAXP, YMINP, YMAXP )
      XDIST = 1.0E+16
      YDIST = 1.0E+16
      DIST = 1.0E+16
      PON_NEAR = 0

      IY = NDAT
      DO 10 IX = 1, NDAT
         IY = IY + 1
         XTEMP = ( XYDATA( IX ) - X ) / ( XMAXP - XMINP )
         YTEMP = ( XYDATA( IY ) - Y ) / ( YMAXP - YMINP )

*  Compare the distance from this point to the smallest distance so far.
         IF ( ( XTEMP**2+YTEMP**2 ) .LT. DIST ) THEN

*  If smaller, save the distances and the index.
            XDIST = XTEMP
            YDIST = YTEMP
            DIST = XTEMP * XTEMP + YTEMP * YTEMP
            PON_NEAR = IX
         END IF
 10   CONTINUE
      END
* $Id$
