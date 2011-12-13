      SUBROUTINE KPS1_CLPTM( BLEDGE, OFFX, OFFY, NPOLY, TICKMAP, X, Y,
     :                       N, STATUS )
*+
*  Name:
*     KPS1_CLPTM

*  Purpose:
*     Draw the tick marks around a cell of a CLINPLOT plot.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CLPTM( BLEDGE, OFFX, OFFY, NPOLY, TICKMAP, X, Y, N,
*                      STATUS )

*  Description:
*     This routine draws the tick marks around a cell in the plot
*     produced by CLINPLOT.

*  Arguments:
*     BLEDGE = LOGICAL (Given)
*        TRUE if the edges of the plotting area are to be left free of
*        tick marks.
*     OFFX = REAL (Given)
*        The offset in X (graphics coords) between the bottom left corner
*        of the cell that was being annotated when TICKMAP was created,
*        and the current cell being annotated.
*     OFFY = REAL (Given)
*        The offset in Y (graphics coords) between the bottom left corner
*        of the cell that was being annotated when TICKMAP was created,
*        and the current cell being annotated.
*     NPOLY = INTEGER (Given)
*        The number of polylines used to draw the tick marks.
*     TICKMAP = INTEGER (Given and Returned)
*        On the initial entry, this should be an AST pointer to a KeyMap
*        holding information about the poly lines used to draw the tick
*        marks for the first cell. This will have been produced by
*        KPG1_ASPLN. The information in this KeyMap is copied out of the
*        KeyMap and into the "X" and "Y" arrays. On exit, this KeyMap is
*        annull and a AST__NULL value is returned.
*     X( * ) = REAL (Given and Returned)
*        Work space used to hold the X coords at each point. If TICKMAP
*        is supplied not equal to AST__NULL, then the contents of TICKMAP
*        are copied into X and returned.
*     Y( * ) = REAL (Given and Returned)
*        Work space used to hold the Y coords at each point. If TICKMAP
*        is supplied not equal to AST__NULL, then the contents of TICKMAP
*        are copied into Y and returned.
*     N( * ) = INTEGER (Given and Returned)
*        Work space used to hold the number of points in each polyline. If
*        TICKMAP is supplied not equal to AST__NULL, then the contents of
*        TICKMAP are copied into Y and returned.
*     STATUS = INTEGER (Given and Returned)
*        Global status value.

*  Copyright:
*     Councils. Copyright (C) 2006 Particle Physics & Astronomy
*     Research Council. All Rights Reserved.

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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-JUN-2006 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      LOGICAL BLEDGE
      REAL OFFX
      REAL OFFY
      INTEGER NPOLY

*  Arguments Returned:
      INTEGER TICKMAP
      REAL X( * )
      REAL Y( * )
      INTEGER N( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER KEY*20          ! KeyMap key for next polyline
      INTEGER I                 ! Polyline index
      INTEGER J                 ! Point index
      INTEGER IAT               ! Used length of a string
      INTEGER IXY               ! Index of start of next polyline
      INTEGER KM                ! Pointer to subsiduary KeyMap
      INTEGER NP                ! No. of points in polyline
      INTEGER NVAL              ! No. of values retrieved from KeyMap
      LOGICAL BAD               ! Does tick touch a blanked edge?
      REAL X1                   ! GRAPHICS X at left hand of Plot
      REAL X2                   ! GRAPHICS X at right hand of Plot
      REAL Y1                   ! GRAPHICS Y at bottom of Plot
      REAL Y2                   ! GRAPHICS Y at top of Plot
*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If a KeyMap holding the polylines used to draw the tick marks has been
*  supplied, copy the information out of the KeyMap into the supplied work
*  arrays.
      IF( TICKMAP .NE. AST__NULL ) THEN

*  Initalise the index of the next free entry in the X/Y work arrays.
         IXY = 1

*  Loop round all the entries in the KeyMap.
         DO I = 1, NPOLY

*  Construct the key for the I'th entry (as created by KPG1_ASPLN).
            KEY = 'POLYLINE'
            IAT = 8
            CALL CHR_PUTI( I, KEY, IAT )

*  Get the subsiduary KeyMap stored with this key.
            IF( AST_MAPGET0A( TICKMAP, KEY, KM, STATUS ) ) THEN

*  Get the number of points in the I'th polyline.
               IF( AST_MAPGET0I( KM, 'N', NP, STATUS ) ) THEN
                  N( I ) = NP

*  Get the X and Y values and append them to the end of the two work arrays.
                  IF( AST_MAPGET1R( KM, 'X', NP, NVAL, X( IXY ),
     :                              STATUS ) .AND.
     :                AST_MAPGET1R( KM, 'Y', NP, NVAL, Y( IXY ),
     :                              STATUS ) ) THEN

*  Increment the index at which to store the next polyline coords.
                     IXY = IXY + NP
                  END IF
               END IF
            END IF
         END DO

*  Now annul the KeyMap pointer and return AST__NULL.
         CALL AST_ANNUL( TICKMAP, STATUS )
      END IF

*  Shift the PGPLOT viewport by the specified values. If the edges are
*  not being blanked, expand the window slightly (0.01 mm at each edge) to
*  ensure graphics drawn at the edge are not acidentally trimmmed due to
*  rounding errors.
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL PGQWIN( X1, X2, Y1, Y2 )
         X1 = X1 - OFFX
         X2 = X2 - OFFX
         Y1 = Y1 - OFFY
         Y2 = Y2 - OFFY
         IF( .NOT. BLEDGE ) THEN
            X1 = X1 - 0.01
            X2 = X2 + 0.01
            Y1 = Y1 - 0.01
            Y2 = Y2 + 0.01
         END IF
         CALL PGSWIN (X1, X2, Y1, Y2)

*  Plot each polyline using PGPLOT.
         IXY = 1
         DO I = 1, NPOLY

*  If we are keeping the edges blank, see if any of the points in the
*  polyline touch the edge.
            BAD = .FALSE.
            IF( BLEDGE ) THEN
               DO J = IXY, IXY + N( I ) - 1
                  IF( ABS( X( J ) - X1 ) .LT. 0.01 .OR.
     :                ABS( X( J ) - X2 ) .LT. 0.01 .OR.
     :                ABS( Y( J ) - Y1 ) .LT. 0.01 .OR.
     :                ABS( Y( J ) - Y2 ) .LT. 0.01 ) THEN
                     BAD = .TRUE.
                     GO TO 10
                  END IF
               END DO
            END IF

 10         IF( .NOT. BAD ) CALL PGLINE( N( I ), X( IXY ), Y( IXY ) )
            IXY = IXY + N( I )

         END DO

*  Shift the PGPLOT window back to its original position.
         IF( .NOT. BLEDGE ) THEN
            X1 = X1 + 0.01
            X2 = X2 - 0.01
            Y1 = Y1 + 0.01
            Y2 = Y2 - 0.01
         END IF

         X1 = X1 + OFFX
         X2 = X2 + OFFX
         Y1 = Y1 + OFFY
         Y2 = Y2 + OFFY
         CALL PGSWIN (X1, X2, Y1, Y2)
      END IF

      END
