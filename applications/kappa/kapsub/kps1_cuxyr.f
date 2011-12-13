      SUBROUTINE KPS1_CUXYR( COSYS, PLOT, NVERT, IPX, IPY, STATUS )
*+
*  Name:
*     KPS1_CUXYR

*  Purpose:
*     Obtains a list of vertex co-ordinates using a graphics cursor.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CUXYR( COSYS, PLOT, NVERT, IPX, IPY, STATUS )

*  Description:
*     Dynamic workspace is reserved to store up to 100 vertices.  The
*     user is then asked to use the cursor to give up to 100 distinct
*     positions in the current picture.  The positions can be marked
*     with crosses or joined by straight lines to form a polygon.  If
*     requested, the world co-ordinates of the cursor positions are
*     transformed into data co-ordinates using the transformation
*     stored in the graphics database.  Pointers to the workspace
*     holding the x and y co-ordinate values are returned.  This
*     workspace should be released by calling PSX_FREE when it is no
*     longer needed.

*  Arguments:
*     COSYS = CHARACTER * ( * ) (Given)
*        The type of co-ordinates which are to be returned, DATA or
*        WORLD.
*     PLOT = CHARACTER * ( * ) (Given)
*        The type of graphics which are to be used to mark the vertices
*        on the graphics device; can be 'POLY' (if the vertices are to
*        be joined by straight lines), 'CROSS' (if the vertices are to
*        be marked by crosses), or 'NONE' (for no graphics).
*     NVERT = INTEGER (Returned)
*        The number of vertices specified.
*     IPX = INTEGER (Returned)
*        A pointer to workspace of type _REAL and size NVERT holding
*        the x co-ordinate of each vertex.
*     IPY = INTEGER (Returned)
*        A pointer to workspace of type _REAL and size NVERT holding
*        the y co-ordinate of each vertex.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     It is assumed that the graphics database for the selected device
*     has been opened, and that the current SGS zone corresponds to
*     the picture from which co-ordinates are required.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     Copyright (C) 1995, 2004 Central Laboratory of the Research
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
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     11-NOV-1993 (DSB):
*        Original version based on KAPPA code.
*     1995 April 11 (MJC):
*        Some tidying of the prologue.  Used modern-style variable
*        declarations.  Called modern KPG1_PRCUR.  Improved the
*        cursor instructions.  Called renamed retrieval routine.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      CHARACTER * ( * ) COSYS
      CHARACTER * ( * ) PLOT

*  Arguments Returned:
      INTEGER NVERT
      INTEGER IPX
      INTEGER IPY

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL MARKSZ                ! Fractional size of the markers
      PARAMETER ( MARKSZ = 0.015 )

      INTEGER MXCHO              ! Maximum number of choices
      PARAMETER ( MXCHO = 3 )

      INTEGER MXVERT             ! Maximum number of vertices allowed
      PARAMETER ( MXVERT = 100 )

*  Local Variables:
      CHARACTER * ( 80 ) CHID( 4 ) ! Commentary on use of image-display
      CHARACTER * ( 80 ) CHTERM( 4 ) ! Commentary on use of terminal
                                 ! cursor
      LOGICAL CURCHO             ! Cursor is available with suitable
                                 ! choices?
      REAL DELTA                 ! Width of markers
      LOGICAL IMGDIS             ! Device is nominally an image display
      INTEGER IPXX               ! Pointer to workspace
      INTEGER IPYY               ! Pointer to workspace
      LOGICAL MARK               ! Crosses required at cursor positions
      LOGICAL POLY               ! Lines required joining cursor
                                 ! positions?
      REAL X1, Y1                ! World co-ords of lower-left corner
      REAL X2, Y2                ! World co-ords of upper-right corner
      REAL XIN, YIN              ! co-ords of the centre of the picture
      REAL XM, YM                ! Size of the image zone in metres

*.

*  Initialise the returned number of vertices.
      NVERT = 0

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the initial position of the cursor to the centre of the current
*  zone.
      CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )
      XIN = 0.5 * ( X1 + X2 )
      YIN = 0.5 * ( Y1 + Y2 )
      CALL SGS_SETCU( XIN, YIN )

*  Calculate the marker height.
      DELTA = MARKSZ * MIN( X2 - X1, Y2 - Y1 )

*  Create some commentary describing how to specify points either with
*  an image display, or with a graphics terminal.
      CHID( 1 ) = 'Use the graphics cursor to define the vertices of '/
     :            /'the next polygon....'
      CHID( 2 ) = '   Press left button on mouse/trackerball to ' //
     :            'select a point.'
      CHID( 3 ) = '   Press right button on mouse/trackerball to ' //
     :            'end input.'
      CHID( 4 ) = ' '

      CHTERM( 1 ) = 'Use the graphics cursor to define the vertices '/
     :              /'of the next polygon...'
      CHTERM( 2 ) = '   Press keyboard "1" or space key to select a '/
     :               /'point.'
      CHTERM( 3 ) = '   Press keyboard "." to end input.'
      CHTERM( 4 ) = ' '

*  Set up the cursor for use.
      CALL KPG1_PRCUR( 1, CHTERM, 4, CHID, 4, '12 .', CURCHO, IMGDIS,
     :                 STATUS )

*  Report an error if no suitable cursor is available.
      IF ( .NOT. CURCHO .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'NOCUR', 'There is no suitable cursor '/
     :                 /'available.', STATUS )
         GO TO 999
      END IF

*  Obtain workspace to hold the vertices.
      CALL PSX_CALLOC( MXVERT, '_REAL', IPX, STATUS )
      CALL PSX_CALLOC( MXVERT, '_REAL', IPY, STATUS )

*  Set flags for the type of graphics required.
      MARK = PLOT .EQ. 'CROSS'
      POLY = PLOT .EQ. 'POLY'

*  Obtain the vertices.
      CALL CURPTS( MXVERT, .FALSE., MXCHO, MARK, .FALSE., DELTA,
     :             POLY, .TRUE., VAL__MINR, VAL__MAXR, VAL__MINR,
     :             VAL__MAXR, XIN, YIN, NVERT, %VAL( CNF_PVAL( IPX ) ),
     :             %VAL( CNF_PVAL( IPY ) ),
     :             STATUS )

*  If the vertices have been joined by straight lines, complete the
*  polygon by joining the first vertex to the last vertex.  The
*  co-ordinates are extracted from the work arrays.
      IF ( POLY .AND. NVERT .GT. 2 ) THEN
         CALL KPG1_RETRR( NVERT, 1, %VAL( CNF_PVAL( IPX ) ),
     :                    X1, STATUS )
         CALL KPG1_RETRR( NVERT, 1, %VAL( CNF_PVAL( IPY ) ),
     :                    Y1, STATUS )
         CALL KPG1_RETRR( NVERT, NVERT, %VAL( CNF_PVAL( IPX ) ),
     :                    X2, STATUS )
         CALL KPG1_RETRR( NVERT, NVERT, %VAL( CNF_PVAL( IPY ) ),
     :                    Y2, STATUS )
         CALL SGS_LINE( X1, Y1, X2, Y2 )
         CALL SGS_FLUSH
      END IF

*  If no vertices were given, annul the workspace.
      IF ( NVERT .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         CALL PSX_FREE( IPX, STATUS )
         CALL PSX_FREE( IPY, STATUS )

*  Otherwise, if data co-ordinates are required, transform the world
*  co-ordinates into data co-ordinates.
      ELSE
         IF ( COSYS .EQ. 'DATA' ) THEN
            CALL PSX_CALLOC( NVERT, '_REAL', IPXX, STATUS )
            CALL PSX_CALLOC( NVERT, '_REAL', IPYY, STATUS )
            CALL AGI_TWTOD( -1, NVERT, %VAL( CNF_PVAL( IPX ) ),
     :                      %VAL( CNF_PVAL( IPY ) ),
     :                      %VAL( CNF_PVAL( IPXX ) ),
     :                      %VAL( CNF_PVAL( IPYY ) ), STATUS )
            CALL PSX_FREE( IPX, STATUS )
            CALL PSX_FREE( IPY, STATUS )
            IPX = IPXX
            IPY = IPYY
         END IF
      END IF

*  If an error has occurred, set the number of vertices in the polygon
*  to zero, and attempt to release the workspace.  This is done within a
*  new error-reporting environment because PSX_FREE does nothing if an
*  error status exists on entry (in contrast to most other `tidying up'
*  routines).
 999  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_BEGIN( STATUS )
         NVERT = 0
         CALL PSX_FREE( IPX, STATUS )
         CALL PSX_FREE( IPY, STATUS )
         CALL ERR_END( STATUS )
      END IF

      END
