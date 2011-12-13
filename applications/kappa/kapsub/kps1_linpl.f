      SUBROUTINE KPS1_LINPL( MODE, EL, XPOS, XWIDTH, YPOS,
     :                       XMIN, XMAX, YMIN, YMAX, XLOG, YLOG,
     :                       LINPEN, SYMBOL, SYMPEN, NEXT, STATUS )
*+
*  Name:
*     KPS1_LINPL

*  Purpose:
*     Plots various kinds of line plot.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_LINPL( MODE, EL, XPOS, XWIDTH, YPOS, XMIN, XMAX, YMIN,
*                      YMAX, XLOG, YLOG, LINPEN, SYMBOL, SYMPEN, NEXT,
*                      STATUS )

*  Description:
*     This routine draws x-y co-ordinate data in a variety of ways
*     within the current SGS zone.  These ways are lines connecting
*     each point; or a symbol plotted at each point; or an histogram,
*     where the y co-ordinate is the height of a bin; or as a series of
*     unconnected lines, whose widths are supplied.  Points where
*     either co-ordinate is bad, are ignored.  Either axis may be
*     logarithmic.
*
*     This routine does not draw axes.

*  Arguments:
*     MODE = CHARACTER * ( * ) (Given)
*        The type of line plot.  This can take the following values.
*           "HISTOGRAM"  An histogram of the points is plotted (with
*                        vertical lines only joining the y values and
*                        not extending to the base of the plot).  The
*                        vertical lines are placed midway between
*                        adjacent x positions.
*           "LINE"       The points are joined by straight lines.
*           "POINT"      A symbol is plotted at each point.  The symbol
*                        is specified by argument SYMBOL and is plotted
*                        with pen SYMPEN.
*           "STEP"       Each point is displayed as a horizontal line,
*                        whose length is specified by the width of the
*                        x position (argument XWIDTH).
*        The value is case insensitive and may be abbreviated.
*     EL = INTEGER (Given)
*        The number of points to be plotted.
*     XPOS( EL ) = REAL (Given)
*        The x co-ordinates of the points to be plotted.
*     XWIDTH( EL ) = REAL (Given)
*        The x co-ordinate widths of the points to be plotted.  This is
*        ignored unless MODE = "STEP".
*     YPOS( EL ) = REAL (Given)
*        The y co-ordinates of the points to be plotted.
*     XMIN = REAL (Given)
*        The lower x co-ordinate bound of the plot.
*     XMAX = REAL (Given)
*        The upper x co-ordinate bound of the plot.
*     YMIN = REAL (Given)
*        The lower y co-ordinate bound of the plot.
*     YMAX = REAL (Given)
*        The upper y co-ordinate bound of the plot.
*     XLOG = LOGICAL (Given)
*        If .TRUE., the x-axis is logarithmic, and thus the base-10
*        logarithms of supplied x co-ordinates are used in plotting.
*     YLOG = LOGICAL (Given)
*        If .TRUE., the y-axis is logarithmic, and thus the base-10
*        logarithms of supplied y co-ordinates are used in plotting.
*     LINPEN = INTEGER (Given)
*        The SGS pen to be used to draw the graph (but not symbols).
*        This should be positive, and preferably in the range 1 to 5 to
*        ensure that there is an available pen.  There is no validation
*        in this routine.
*     SYMBOL = INTEGER (Given)
*        The symbol for plotting the point.  Must in the range 1 to 5.
*     SYMPEN = INTEGER (Given)
*        The SGS pen to be used to draw the symbol.  This should be
*        positive, and preferably in the range 1 to 5 to ensure that
*        there is an available pen.  There is no validation in this
*        routine.
*     NEXT( EL ) = INTEGER (Returned)
*        A work array only used when MODE = "HISTOGRAM".  It is used to
*        store the index of the next valid x co-ordinate.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     An SGS workstation must be open.

*  Copyright:
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 September 27 (MJC):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      CHARACTER * ( * ) MODE
      INTEGER EL
      REAL XPOS( EL )
      REAL XWIDTH( EL )
      REAL YPOS( EL )
      REAL XMIN
      REAL XMAX
      REAL YMIN
      REAL YMAX
      LOGICAL XLOG
      LOGICAL YLOG
      INTEGER LINPEN
      INTEGER SYMBOL
      INTEGER SYMPEN

*  Arguments Returned:
      INTEGER NEXT( EL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL DX                    ! Separation in x of adjacent points
      INTEGER I                  ! Loop counter
      INTEGER J                  ! Loop counter
      INTEGER K                  ! Loop counter
      LOGICAL LOOP               ! Loop to search for valid co-ordinate?
      INTEGER PEN                ! Current SGS pen
      LOGICAL START              ! A polyline has been started
      INTEGER SYMB               ! Constrained symbol number
      CHARACTER * ( 10 ) TYPE    ! Uppercase version of MODE
      REAL X                     ! X position after manipulation
      REAL XL                    ! Start x position of a step
      REAL XLOW                  ! Minimum x position
      REAL XLAST                 ! Previous x position
      REAL XU                    ! End x position of a step
      REAL Y                     ! Y position after manipulation
      REAL YLOW                  ! Minimum y position

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the lower limiting co-ordinates for logarithmic plots.
*  Allow for a reversed co-ordinate axis.
      XLOW = MAX( VAL__SMLR, MIN( XMIN, XMAX ) )
      YLOW = MAX( VAL__SMLR, YMIN )

*  Note that the plot has yet to start.
      START = .FALSE.

*  Convert the plot type to uppercase.
      TYPE = MODE
      CALL CHR_LDBLK( TYPE )
      CALL CHR_UCASE( TYPE )

*  Inquire current SGS pen.
      CALL SGS_IPEN( PEN )


*  Line plot.
*  ==========
      IF ( TYPE( 1:1 ) .EQ. 'L' ) THEN

*  Draw the lines with the requested pen.
         CALL SGS_SPEN( LINPEN )

*  Deal with no logarithms first.
*  ------------------------------
         IF ( .NOT. XLOG .AND. .NOT. YLOG ) THEN

*  Loop through every point forming a polyline.  Do not plot when
*  either co-ordinate is bad.
            DO I = 1, EL
               IF ( XPOS( I ) .NE. VAL__BADR .AND.
     :              YPOS( I ) .NE. VAL__BADR ) THEN

                  IF ( START ) THEN
                     CALL SGS_APOLY( XPOS( I ), YPOS( I ) )
                  ELSE
                     CALL SGS_BPOLY( XPOS( I ), YPOS( I ) )
                     START = .TRUE.
                  END IF
               END IF
            END DO

*  Deal with both logarithmic axes.
*  --------------------------------
         ELSE IF ( XLOG .AND. YLOG ) THEN

*  Loop through every point forming a polyline of the logarithmic
*  co-ordinates.  Do not plot when either co-ordinate is bad or lies
*  outside the plot or is not positive.
            DO I = 1, EL
               IF ( XPOS( I ) .NE. VAL__BADR .AND.
     :              YPOS( I ) .NE. VAL__BADR .AND.
     :              XPOS( I ) .GE. XLOW .AND.
     :              YPOS( I ) .GE. YLOW ) THEN

                  X = LOG10( XPOS( I ) )
                  Y = LOG10( YPOS( I ) )
                  IF ( START ) THEN
                     CALL SGS_APOLY( X, Y )
                  ELSE
                     CALL SGS_BPOLY( X, Y )
                     START = .TRUE.
                  END IF
               END IF
            END DO

*  Deal with x logarithmic axis.
*  -----------------------------
         ELSE IF ( XLOG ) THEN

*  Loop through every point forming a polyline of the co-ordinates.  Do
*  not plot when either co-ordinate is bad or log x co-ordinate lies
*  outside the plot or is not positive.
            DO I = 1, EL
               IF ( XPOS( I ) .NE. VAL__BADR .AND.
     :              YPOS( I ) .NE. VAL__BADR .AND.
     :              XPOS( I ) .GE. XLOW ) THEN

                  X = LOG10( XPOS( I ) )
                  IF ( START ) THEN
                     CALL SGS_APOLY( X, YPOS( I ) )
                  ELSE
                     CALL SGS_BPOLY( X, YPOS( I ) )
                     START = .TRUE.
                  END IF
               END IF
            END DO

*  Deal with y logarithmic axis.
*  -----------------------------
         ELSE IF ( YLOG ) THEN

*  Loop through every point forming a polyline of the logarithmic
*  co-ordinates.  Do not plot when either co-ordinate is bad or lies
*  outside the plot or is not positive.
            DO I = 1, EL
               IF ( XPOS( I ) .NE. VAL__BADR .AND.
     :              YPOS( I ) .NE. VAL__BADR .AND.
     :              YPOS( I ) .GE. YLOW ) THEN

                  Y = LOG10( YPOS( I ) )
                  IF ( START ) THEN
                     CALL SGS_APOLY( XPOS( I ), Y )
                  ELSE
                     CALL SGS_BPOLY( XPOS( I ), Y )
                     START = .TRUE.
                  END IF
               END IF
            END DO
         END IF

*  Flush the polyline buffer.
         CALL SGS_OPOLY

*  Point plot.
*  ===========
      ELSE IF ( TYPE( 1:1 ) .EQ. 'P' ) THEN

*  Only sybols 1 to 5 are acceptable in SGS.
         SYMB = MAX( 1, MIN( 5, SYMBOL ) )

*  Draw the symbols with the requested pen.
         CALL SGS_SPEN( SYMPEN )

*  Deal with no logarithms.
*  ------------------------
         IF ( .NOT. XLOG .AND. .NOT. YLOG ) THEN

*  Loop through every point plotting a symbol.  Do not plot when
*  either co-ordinate is bad.
            DO I = 1, EL
               IF ( XPOS( I ) .NE. VAL__BADR .AND.
     :              YPOS( I ) .NE. VAL__BADR ) THEN

                  CALL SGS_MARK( XPOS( I ), YPOS( I ), SYMB )
               END IF
            END DO

*  Deal with both logarithmic axes.
*  --------------------------------
         ELSE IF ( XLOG .AND. YLOG ) THEN

*  Loop through every point plotting a symbol at the logarithmic
*  co-ordinates.  Do not plot when either co-ordinate is bad or lies
*  outside the plot or is not positive.
            DO I = 1, EL
               IF ( XPOS( I ) .NE. VAL__BADR .AND.
     :              YPOS( I ) .NE. VAL__BADR .AND.
     :              XPOS( I ) .GE. XLOW .AND.
     :              YPOS( I ) .GE. YLOW ) THEN

                  X = LOG10( XPOS( I ) )
                  Y = LOG10( YPOS( I ) )
                  CALL SGS_MARK( X, Y, SYMB )
               END IF
            END DO

*  Deal with x logarithmic axis.
*  -----------------------------
         ELSE IF ( XLOG ) THEN

*  Loop through every point drawing a symbol at log-linear
*  co-ordinates.  Do not plot when either co-ordinate is bad or log x
*  co-ordinate lies outside the plot or is not positive.
            DO I = 1, EL
               IF ( XPOS( I ) .NE. VAL__BADR .AND.
     :              YPOS( I ) .NE. VAL__BADR .AND.
     :              XPOS( I ) .GE. XLOW ) THEN

                  X = LOG10( XPOS( I ) )
                  CALL SGS_MARK( X, YPOS( I ), SYMB )
               END IF
            END DO

*  Deal with y logarithmic axis.
*  -----------------------------
         ELSE IF ( YLOG ) THEN

*  Loop through every point forming a polyline of the logarithmic
*  co-ordinates.  Do not plot when either co-ordinate is bad or lies
*  outside the plot or is not positive.
            DO I = 1, EL
               IF ( XPOS( I ) .NE. VAL__BADR .AND.
     :              YPOS( I ) .NE. VAL__BADR .AND.
     :              YPOS( I ) .GE. YLOW ) THEN

                  Y = LOG10( YPOS( I ) )
                  CALL SGS_MARK( XPOS( I ), Y, SYMB )
               END IF
            END DO
         END IF

*  Restore the previous current SGS pen.
         CALL SGS_SPEN( PEN )

*  Step plot.
*  ==========
      ELSE IF ( TYPE( 1:1 ) .EQ. 'S' ) THEN

*  Draw the lines with the requested pen.
         CALL SGS_SPEN( LINPEN )

*  Deal with no logarithms first.
*  ------------------------------
         IF ( .NOT. XLOG .AND. .NOT. YLOG ) THEN

*  Loop through every point drawing a horizontal line of the specified
*  width symmetric about the x co-ordinate.  Do not plot when either
*  co-ordinate or the width is bad.
            DO I = 1, EL
               IF ( XPOS( I ) .NE. VAL__BADR .AND.
     :              YPOS( I ) .NE. VAL__BADR .AND.
     :              XWIDTH( I ) .NE. VAL__BADR  ) THEN

                  CALL SGS_LINE( XPOS( I ) - 0.5 * XWIDTH( I ),
     :                           YPOS( I ),
     :                           XPOS( I ) + 0.5 * XWIDTH( I ),
     :                           YPOS( I ) )
               END IF
            END DO

*  Deal with both logarithmic axes.
*  --------------------------------
         ELSE IF ( XLOG .AND. YLOG ) THEN

*  Loop through every point drawing a horizontal line of the specified
*  width symmetric about the x co-ordinate.  Do not plot when either of
*  the co-ordinates is bad, or when the width is bad.
            DO I = 1, EL
               IF ( XPOS( I ) .NE. VAL__BADR .AND.
     :              YPOS( I ) .NE. VAL__BADR .AND.
     :              XWIDTH( I ) .NE. VAL__BADR ) THEN

*  Calculate the x-limits for each line segment.
                  XL = XPOS( I ) - 0.5 * XWIDTH( I )
                  XU = XPOS( I ) + 0.5 * XWIDTH( I )

*  Do not plot if any of the locus lies outside the plot or is not
*  positive.
                  IF ( XL .GE. XLOW .AND. XU .GE. XLOW .AND.
     :                 YPOS( I ) .GE. YLOW ) THEN

*  Calculate and plot the locus for the current element.
                     XL = LOG10( XL )
                     XU = LOG10( XU )
                     Y = LOG10( YPOS( I ) )
                     CALL SGS_LINE( XL, Y, XU, Y )
                  END IF
               END IF
            END DO

*  Deal with x logarithmic axis.
*  -----------------------------
         ELSE IF ( XLOG ) THEN

*  Loop through every point drawing a horizontal line of the specified
*  width symmetric about the x co-ordinate.  Do not plot when either of
*  the co-ordinates is bad, or when the width is bad.
            DO I = 1, EL
               IF ( XPOS( I ) .NE. VAL__BADR .AND.
     :              YPOS( I ) .NE. VAL__BADR .AND.
     :              XWIDTH( I ) .NE. VAL__BADR ) THEN

*  Calculate the x-limits for each line segment.
                  XL = XPOS( I ) - 0.5 * XWIDTH( I )
                  XU = XPOS( I ) + 0.5 * XWIDTH( I )

*  Do not plot if any of the locus lies outside the plot or is not
*  positive.
                  IF ( XL .GE. XLOW .AND. XU .GE. XLOW ) THEN

*  Calculate and plot the locus for the current element.
                     XL = LOG10( XL )
                     XU = LOG10( XU )
                     CALL SGS_LINE( XL, YPOS( I ), XU, YPOS( I ) )
                  END IF
               END IF
            END DO


*  Deal with y logarithmic axis.
*  -----------------------------
         ELSE IF ( YLOG ) THEN

*  Loop through every point drawing a horizontal line of the specified
*  width symmetric about the x co-ordinate.  Do not plot when either
*  co-ordinate or the width is bad, or the y co-ordinate lies outside
*  the plot.
            DO I = 1, EL
               IF ( XPOS( I ) .NE. VAL__BADR .AND.
     :              YPOS( I ) .NE. VAL__BADR .AND.
     :              XWIDTH( I ) .NE. VAL__BADR .AND.
     :              YPOS( I ) .GE. YLOW ) THEN

                  CALL SGS_LINE( XPOS( I ) - 0.5 * XWIDTH( I ),
     :                           LOG10( YPOS( I ) ),
     :                           XPOS( I ) + 0.5 * XWIDTH( I ),
     :                           LOG10( YPOS( I ) ) )
               END IF
            END DO

         END IF

*  Histogram plot.
*  ===============
      ELSE IF ( TYPE( 1:1 ) .EQ. 'H' ) THEN

*  Draw the lines with the requested pen.
         CALL SGS_SPEN( LINPEN )

*  Find the first point with a valid x co-ordinate.
         I = 1
         LOOP = .TRUE.
         DO WHILE ( I .LE. EL .AND. LOOP )
            IF ( XPOS( I ) .NE. VAL__BADR ) THEN
               LOOP = .FALSE.
            ELSE
               I = I + 1
            END IF
         END DO

*  Report an error there is no point with a valid x co-ordinate.
         IF ( LOOP ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'KPS1_LINPL_BADX',
     :        'The x co-ordinate data are all bad (possible '/
     :        /'programming error),', STATUS )
            GOTO 999
         END IF

*  Obtain the indices to the next valid x co-ordinate.
         DO WHILE ( I .LT. EL )

*  Find the next pixel with a valid x co-ordinate.
            J = I + 1
            LOOP = .TRUE.
            DO WHILE ( J .LE. EL .AND. LOOP )
               IF ( XPOS( J ) .NE. VAL__BADR ) THEN
                  LOOP = .FALSE.
               ELSE
                  J = J + 1
               END IF
            END DO

*  Report an error there is no point with a valid x co-ordinate.
            IF ( LOOP ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'KPS1_LINPL_ONEX',
     :           'There is only one valid x co-ordinate (possible '/
     :           /'programming error),', STATUS )
               GOTO 999
            END IF

*  Keep a record of the next valid index for all the points from the
*  current point to immediately preceeding the next valid index.
            DO K = I, J - 1
               NEXT( K ) = J
            END DO

*  Increment the index counter to the next valid point.
            I = J
         END DO

*  Deal with no logarithms first.
*  ------------------------------
         IF ( .NOT. XLOG .AND. .NOT. YLOG ) THEN

*  Loop through every point forming a polyline.  Do not plot when the x
*  co-ordinate is bad.  Set a bad y value to the minimum y in the plot.
            DO I = 1, EL
               IF ( XPOS( I ) .NE. VAL__BADR ) THEN
                  IF ( YPOS( I ) .EQ. VAL__BADR ) THEN
                     Y = YMIN
                  ELSE
                     Y = YPOS( I )
                  END IF

*  First time through...
                  IF ( .NOT. START ) THEN

*  Find the length of the initial line, and plot it.
                     DX = XPOS( NEXT( I ) ) - XPOS( I )
                     CALL SGS_BPOLY( XPOS( I ) - 0.5 * DX, Y )
                     XLAST = XPOS( I ) + 0.5 * DX
                     CALL SGS_APOLY( XLAST, Y )

                     START = .TRUE.

*  Subsequent points...
                  ELSE

*  Plot the vertical line.
                     CALL SGS_APOLY( XLAST, Y )

*  Find the length of the line, and plot it.  The point uses the
*  previous width for the last point.
                     IF ( I .LT. EL ) DX = XPOS( NEXT( I ) ) - XPOS( I )
                     XLAST = XPOS( I ) + 0.5 * DX
                     CALL SGS_APOLY( XLAST, Y )
                  END IF
               END IF
            END DO

*  Deal with both logarithmic axes.
*  --------------------------------
         ELSE IF ( XLOG .AND. YLOG ) THEN

*  Loop through every point forming a polyline of the logarithmic
*  co-ordinates.  Do not plot when either the x co-ordinate is bad, or
*  lies outside the plot or is not positive.  Set a bad y value to the
*  minimum y in the plot.
            DO I = 1, EL
               IF ( XPOS( I ) .NE. VAL__BADR ) THEN
                  IF ( YPOS( I ) .EQ. VAL__BADR .OR.
     :                 YPOS( I ) .LT. YLOW ) THEN
                     Y = LOG10( YMIN )
                  ELSE
                     Y = LOG10( YPOS( I ) )
                  END IF

*  First time through...
                  IF ( .NOT. START ) THEN

*  Find the length of the initial line, and plot it.
                     DX = XPOS( NEXT( I ) ) - XPOS( I )
                     X = LOG10( MAX( XLOW, XPOS( I ) - 0.5 * DX ) )
                     CALL SGS_BPOLY( X, Y )
                     XLAST = LOG10( MAX( XLOW, XPOS( I ) - 0.5 * DX ) )
                     CALL SGS_APOLY( XLAST, Y )

                     START = .TRUE.

*  Subsequent points...
                  ELSE

*  Plot the vertical line.
                     CALL SGS_APOLY( XLAST, Y )

*  Find the length of the line, and plot it.  The point uses the
*  previous width for the last point.
                     IF ( I .LT. EL ) DX = XPOS( NEXT( I ) ) - XPOS( I )
                     XLAST = LOG10( MAX( XLOW, XPOS( I ) + 0.5 * DX ) )
                     CALL SGS_APOLY( XLAST, Y )
                  END IF
               END IF
            END DO

*  Deal with x logarithmic axis.
*  -----------------------------
         ELSE IF ( XLOG ) THEN

*  Loop through every point forming a polyline of the logarithmic
*  co-ordinates.  Do not plot when either the x co-ordinate is bad, or
*  lies outside the plot or is not positive.  Set a bad y value to the
*  minimum y in the plot.
            DO I = 1, EL
               IF ( XPOS( I ) .NE. VAL__BADR ) THEN
                  IF ( YPOS( I ) .EQ. VAL__BADR ) THEN
                     Y = YMIN
                  ELSE
                     Y = YPOS( I )
                  END IF

*  First time through...
                  IF ( .NOT. START ) THEN

*  Find the length of the initial line, and plot it.
                     DX = XPOS( NEXT( I ) ) - XPOS( I )
                     X = LOG10( MAX( XLOW, XPOS( I ) - 0.5 * DX ) )
                     CALL SGS_BPOLY( X, Y )
                     XLAST = LOG10( MAX( XLOW, XPOS( I ) - 0.5 * DX ) )
                     CALL SGS_APOLY( XLAST, Y )

                     START = .TRUE.

*  Subsequent points...
                  ELSE

*  Plot the vertical line.
                     CALL SGS_APOLY( XLAST, Y )

*  Find the length of the line, and plot it.  The point uses the
*  previous width for the last point.
                     IF ( I .LT. EL ) DX = XPOS( NEXT( I ) ) - XPOS( I )
                     XLAST = LOG10( MAX( XLOW, XPOS( I ) + 0.5 * DX ) )
                     CALL SGS_APOLY( XLAST, Y )
                  END IF
               END IF
            END DO

*  Deal with y logarithmic axis.
*  -----------------------------
         ELSE IF ( YLOG ) THEN

*  Loop through every point forming a polyline of the logarithmic
*  co-ordinates.  Do not plot when either the x co-ordinate is bad, or
*  lies outside the plot or is not positive.  Set a bad y value to the
*  minimum y in the plot.
            DO I = 1, EL
               IF ( XPOS( I ) .NE. VAL__BADR ) THEN
                  IF ( YPOS( I ) .EQ. VAL__BADR .OR.
     :                 YPOS( I ) .LT. YLOW ) THEN
                     Y = LOG10( YMIN )
                  ELSE
                     Y = LOG10( YPOS( I ) )
                  END IF

*  First time through...
                  IF ( .NOT. START ) THEN

*  Find the length of the initial line, and plot it.
                     DX = XPOS( NEXT( I ) ) - XPOS( I )
                     X = MAX( XLOW, XPOS( I ) - 0.5 * DX )
                     CALL SGS_BPOLY( X, Y )
                     XLAST = MAX( XLOW, XPOS( I ) - 0.5 * DX )
                     CALL SGS_APOLY( XLAST, Y )

                     START = .TRUE.

*  Subsequent points...
                  ELSE

*  Plot the vertical line.
                     CALL SGS_APOLY( XLAST, Y )

*  Find the length of the line, and plot it.  The point uses the
*  previous width for the last point.
                     IF ( I .LT. EL ) DX = XPOS( NEXT( I ) ) - XPOS( I )
                     XLAST = MAX( XLOW, XPOS( I ) + 0.5 * DX )
                     CALL SGS_APOLY( XLAST, Y )
                  END IF
               END IF
            END DO
         END IF

*  Flush the polyline buffer.
         CALL SGS_OPOLY

      END IF

  999 CONTINUE

*  Restore the previous current SGS pen.
      CALL SGS_SPEN( PEN )

*  Flush the plotting buffer.
      CALL SGS_FLUSH

      END
