      SUBROUTINE KPG1_ERBAR( ERRTYP, ERRPEN, FREQ, EL, XPOS, XERROR,
     :                       YPOS, YERROR, XMIN, XMAX, YMIN, YMAX,
     :                       XLOG, YLOG, STATUS )
*+
*  Name:
*     KPG1_ERBAR

*  Purpose:
*     Plots various kinds of error bar.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ERBAR( ERRTYP, ERRPEN, FREQ, EL, XPOS, XERROR,
*                      YPOS, YERROR, XMIN, XMAX, YMIN, YMAX,
*                      XLOG, YLOG, STATUS )

*  Description:
*     This routine plots error bars for an array of x-y co-ordinates.
*     Argument ERRTYP specifies the appearance of the error bars, and
*     ERRPEN the pen used to plot them.  The frequency of error bars is
*     also controllable through argument FREQ.  Either axis may be
*     logarithmic.

*  Arguments:
*     ERRTYP = CHARACTER * ( * ) (Given)
*        The way the errors are to be represented graphically.  ERRTYP
*        can take the following values.
*           "BARS"     A cross with serifs is plotted joining the x
*                      error limits and then the y error limits.
*           "CROSS"    A san-serif cross is plotted joining the x error
*                      limits and then the y error limits.
*           "DIAMOND"  Adjacent error limits are joined to form a error
*                      diamond.
*        The value is case insensitive and may be abbreviated.
*     ERRPEN = INTEGER (Given)
*        The pen to be used to draw the error bars or diamond.  This
*        should be positive, and preferably in the range 1 to 5 to
*        ensure that there is an available pen.  There is no validation
*        in this routine.
*     FREQ = INTEGER (Given)
*        The frequency at which error bars are to be plotted.  So a
*        value of 2 would mean that alternative points have error bars
*        plotted.
*     EL = INTEGER (Given)
*        The number of points to be plotted.
*     XPOS( EL ) = REAL (Given)
*        The x co-ordinates of the points to be plotted.
*     XERROR( EL ) = REAL (Given)
*        The x co-ordinate errors of the points to be plotted.
*     YPOS( EL ) = REAL (Given)
*        The y co-ordinates of the points to be plotted.
*     YERROR( EL ) = REAL (Given)
*        The y co-ordinate errors of the points to be plotted.
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
*     [argument_spec]...
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     An SGS workstation must be open.

*  [optional_subroutine_items]...
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
*     useful,but WITHOUT ANY WARRANTY; without even the implied
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
*     1996 September 30 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      CHARACTER * ( * ) ERRTYP
      INTEGER ERRPEN
      INTEGER FREQ
      INTEGER EL
      REAL XPOS( EL )
      REAL XERROR( EL )
      REAL YPOS( EL )
      REAL YERROR( EL )
      REAL XMIN
      REAL XMAX
      REAL YMIN
      REAL YMAX
      LOGICAL XLOG
      LOGICAL YLOG

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER CFREQ              ! Constrained frequency of error shapes
      REAL DX                    ! Tick length along x axis
      REAL DY                    ! Tick length along y axis
      INTEGER I                  ! Loop counter
      INTEGER PEN                ! Current SGS pen
      REAL TICKM                 ! Tick size in metres
      CHARACTER * ( 8 ) TYPE     ! Uppercase version of ERRTYP
      REAL X                     ! X position after manipulation
      REAL X1                    ! Lower x bound of the current zone
      REAL X2                    ! Upper x bound of the current zone
      REAL XL                    ! Lower x limit of error shape
      REAL XLOW                  ! Minimum x position
      REAL XM                    ! Current zone x size in metres
      REAL XU                    ! Upper x limit of error shape
      REAL Y                     ! Y position after manipulation
      REAL Y1                    ! Lower y bound of the current zone
      REAL Y2                    ! Upper y bound of the current zone
      REAL YL                    ! Lower y limit of error shape
      REAL YLOW                  ! Minimum y position
      REAL YM                    ! Current zone y size in metres
      REAL YU                    ! Upper y limit of error shape

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the lower limiting co-ordinates for logarithmic plots.
*  Allow for a reversed co-ordinate axis.
       XLOW = MAX( VAL__SMLR, MIN( XMIN, XMAX ) )
       YLOW = MAX( VAL__SMLR, YMIN )

*  Constrain the frequency to be positive and give at least one
*  error bar.
       CFREQ = MAX( 1, MIN( EL, FREQ ) )

*  Convert the error-bar type to uppercase.
       TYPE = ERRTYP
       CALL CHR_LDBLK( TYPE )
       CALL CHR_UCASE( TYPE )

*  Inquire current SGS pen.
       CALL SGS_IPEN( PEN )

*  Draw the error shapes with the requested pen.
       CALL SGS_SPEN( ERRPEN )

*  Diamond error bars.
*  ===================
      IF ( TYPE( 1:1 ) .EQ. 'D' ) THEN

*  Deal with no logarithms first.
*  ------------------------------
         IF ( .NOT. XLOG .AND. .NOT. YLOG ) THEN

*  Loop through the points at the desired frequency.  Do not plot when
*  either co-ordinate is bad.
            DO I = 1, EL, CFREQ
               IF ( XPOS( I ) .NE. VAL__BADR .AND.
     :              YPOS( I ) .NE. VAL__BADR ) THEN

*  Calculate the limits of the errors for the point.  Allow for bad
*  values.
                  IF ( XERROR( I ) .EQ. VAL__BADR ) THEN
                     XL = XPOS( I )
                     XU = XPOS( I )
                  ELSE
                     XL = XPOS( I ) - XERROR( I )
                     XU = XPOS( I ) + XERROR( I )
                  END IF
                  IF ( YERROR( I ) .EQ. VAL__BADR ) THEN
                     YL = YPOS( I )
                     YU = YPOS( I )
                  ELSE
                     YL = YPOS( I ) - YERROR( I )
                     YU = YPOS( I ) + YERROR( I )
                  END IF

*  Draw the diamond.
                  CALL SGS_LINE( XPOS( I ), YL, XU, YPOS( I ) )
                  CALL SGS_LINE( XU, YPOS( I ), XPOS( I ), YU )
                  CALL SGS_LINE( XPOS( I ), YU, XL, YPOS( I ) )
                  CALL SGS_LINE( XL, YPOS( I ), XPOS( I ), YL )
               END IF
            END DO

*  Deal with both logarithmic axes.
*  --------------------------------
         ELSE IF ( XLOG .AND. YLOG ) THEN

*  Loop through the points at the desired frequency.  Do not plot when
*  either co-ordinate is bad.
            DO I = 1, EL, CFREQ
               IF ( XPOS( I ) .NE. VAL__BADR .AND.
     :              YPOS( I ) .NE. VAL__BADR ) THEN

*  Calculate the limits of the errors for the point.  Allow for bad
*  values.
                  IF ( XERROR( I ) .EQ. VAL__BADR ) THEN
                     XL = XPOS( I )
                     XU = XPOS( I )
                  ELSE
                     XL = XPOS( I ) - XERROR( I )
                     XU = XPOS( I ) + XERROR( I )
                  END IF
                  IF ( YERROR( I ) .EQ. VAL__BADR ) THEN
                     YL = YPOS( I )
                     YU = YPOS( I )
                  ELSE
                     YL = YPOS( I ) - YERROR( I )
                     YU = YPOS( I ) + YERROR( I )
                  END IF

*  Check that these are valid values (able to take a logarithm).
                  IF ( XL .GT. XLOW .AND. YL .GT. YLOW ) THEN

*  Draw the diamond.
                     X = LOG10( XPOS( I ) )
                     Y = LOG10( YPOS( I ) )
                     CALL SGS_LINE( X, LOG10( YL ), LOG10( XU ), Y )
                     CALL SGS_LINE( LOG10( XU ), Y, X, LOG10( YU ) )
                     CALL SGS_LINE( X, LOG10( YU ), LOG10( XL ), Y )
                     CALL SGS_LINE( LOG10( XL ), Y, X, LOG10( YL ) )
                  END IF
               END IF
            END DO

*  Deal with x logarithmic axis.
*  -----------------------------
         ELSE IF ( XLOG ) THEN

*  Loop through the points at the desired frequency.  Do not plot when
*  either co-ordinate is bad.
            DO I = 1, EL, CFREQ
               IF ( XPOS( I ) .NE. VAL__BADR .AND.
     :              YPOS( I ) .NE. VAL__BADR ) THEN

*  Calculate the limits of the errors for the point.  Allow for bad
*  values.
                  IF ( XERROR( I ) .EQ. VAL__BADR ) THEN
                     XL = XPOS( I )
                     XU = XPOS( I )
                  ELSE
                     XL = XPOS( I ) - XERROR( I )
                     XU = XPOS( I ) + XERROR( I )
                  END IF
                  IF ( YERROR( I ) .EQ. VAL__BADR ) THEN
                     YL = YPOS( I )
                     YU = YPOS( I )
                  ELSE
                     YL = YPOS( I ) - YERROR( I )
                     YU = YPOS( I ) + YERROR( I )
                  END IF

*  Check that the x co-ordinates are valid values (able to take a
*  logarithm).
                  IF ( XL .GT. XLOW ) THEN

*  Draw the diamond.
                     X = LOG10( XPOS( I ) )
                     CALL SGS_LINE( X, YL, LOG10( XU ), YPOS( I ) )
                     CALL SGS_LINE( LOG10( XU ), YPOS( I ), X, YU )
                     CALL SGS_LINE( X, YU, LOG10( XL ), YPOS( I ) )
                     CALL SGS_LINE( LOG10( XL ), YPOS( I ), X, YL )
                  END IF
               END IF
            END DO

*  Deal with y logarithmic axis.
*  -----------------------------
         ELSE IF ( YLOG ) THEN

*  Loop through the points at the desired frequency.  Do not plot when
*  either co-ordinate is bad.
            DO I = 1, EL, CFREQ
               IF ( XPOS( I ) .NE. VAL__BADR .AND.
     :              YPOS( I ) .NE. VAL__BADR ) THEN

*  Calculate the limits of the errors for the point.  Allow for bad
*  values.
                  IF ( XERROR( I ) .EQ. VAL__BADR ) THEN
                     XL = XPOS( I )
                     XU = XPOS( I )
                  ELSE
                     XL = XPOS( I ) - XERROR( I )
                     XU = XPOS( I ) + XERROR( I )
                  END IF
                  IF ( YERROR( I ) .EQ. VAL__BADR ) THEN
                     YL = YPOS( I )
                     YU = YPOS( I )
                  ELSE
                     YL = YPOS( I ) - YERROR( I )
                     YU = YPOS( I ) + YERROR( I )
                  END IF

*  Check that the y co-ordinates are valid values (able to take a
*  logarithm).
                  IF ( YL .GT. YLOW ) THEN

*  Draw the diamond.
                     Y = LOG10( YPOS( I ) )
                     CALL SGS_LINE( XPOS( I ), LOG10( YL ), XU, Y )
                     CALL SGS_LINE( XU, Y, XPOS( I ), LOG10( YU ) )
                     CALL SGS_LINE( XPOS( I ), LOG10( YU ), XL, Y )
                     CALL SGS_LINE( XL, Y, XPOS( I ), LOG10( YL ) )
                  END IF
               END IF
            END DO
         END IF

*  San-serif cross error bars.
*  ===========================
      ELSE IF ( TYPE( 1:1 ) .EQ. 'C' ) THEN

*  Deal with no logarithms first.
*  ------------------------------
         IF ( .NOT. XLOG .AND. .NOT. YLOG ) THEN

*  Loop through the points at the desired frequency.  Do not plot when
*  either co-ordinate is bad.
            DO I = 1, EL, CFREQ
               IF ( XPOS( I ) .NE. VAL__BADR .AND.
     :              YPOS( I ) .NE. VAL__BADR ) THEN

*  Calculate the limits of the errors for the point.  Allow for bad
*  values.
                  IF ( XERROR( I ) .EQ. VAL__BADR ) THEN
                     XL = XPOS( I )
                     XU = XPOS( I )
                  ELSE
                     XL = XPOS( I ) - XERROR( I )
                     XU = XPOS( I ) + XERROR( I )
                  END IF
                  IF ( YERROR( I ) .EQ. VAL__BADR ) THEN
                     YL = YPOS( I )
                     YU = YPOS( I )
                  ELSE
                     YL = YPOS( I ) - YERROR( I )
                     YU = YPOS( I ) + YERROR( I )
                  END IF

*  Draw the cross.
                  CALL SGS_LINE( XL, YPOS( I ), XU, YPOS( I ) )
                  CALL SGS_LINE( XPOS( I ), YL, XPOS( I ), YU )
               END IF
            END DO

*  Deal with both logarithmic axes.
*  --------------------------------
         ELSE IF ( XLOG .AND. YLOG ) THEN

*  Loop through the points at the desired frequency.  Do not plot when
*  either co-ordinate is bad.
            DO I = 1, EL, CFREQ
               IF ( XPOS( I ) .NE. VAL__BADR .AND.
     :              YPOS( I ) .NE. VAL__BADR ) THEN

*  Calculate the limits of the errors for the point.  Allow for bad
*  values.
                  IF ( XERROR( I ) .EQ. VAL__BADR ) THEN
                     XL = XPOS( I )
                     XU = XPOS( I )
                  ELSE
                     XL = XPOS( I ) - XERROR( I )
                     XU = XPOS( I ) + XERROR( I )
                  END IF
                  IF ( YERROR( I ) .EQ. VAL__BADR ) THEN
                     YL = YPOS( I )
                     YU = YPOS( I )
                  ELSE
                     YL = YPOS( I ) - YERROR( I )
                     YU = YPOS( I ) + YERROR( I )
                  END IF

*  Check that these are valid values (able to take a logarithm).
                  IF ( XL .GT. XLOW .AND. YL .GT. YLOW ) THEN

*  Draw the cross.
                     X = LOG10( XPOS( I ) )
                     Y = LOG10( YPOS( I ) )
                     CALL SGS_LINE( LOG10( XL ), Y, LOG10( XU ), Y )
                     CALL SGS_LINE( X, LOG10( YL ), X, LOG10( YU ) )
                  END IF
               END IF
            END DO

*  Deal with x logarithmic axis.
*  -----------------------------
         ELSE IF ( XLOG ) THEN

*  Loop through the points at the desired frequency.  Do not plot when
*  either co-ordinate is bad.
            DO I = 1, EL, CFREQ
               IF ( XPOS( I ) .NE. VAL__BADR .AND.
     :              YPOS( I ) .NE. VAL__BADR ) THEN

*  Calculate the limits of the errors for the point.  Allow for bad
*  values.
                  IF ( XERROR( I ) .EQ. VAL__BADR ) THEN
                     XL = XPOS( I )
                     XU = XPOS( I )
                  ELSE
                     XL = XPOS( I ) - XERROR( I )
                     XU = XPOS( I ) + XERROR( I )
                  END IF
                  IF ( YERROR( I ) .EQ. VAL__BADR ) THEN
                     YL = YPOS( I )
                     YU = YPOS( I )
                  ELSE
                     YL = YPOS( I ) - YERROR( I )
                     YU = YPOS( I ) + YERROR( I )
                  END IF

*  Check that the x co-ordinates are valid values (able to take a
*  logarithm).
                  IF ( XL .GT. XLOW ) THEN

*  Draw the cross.
                     X = LOG10( XPOS( I ) )
                     CALL SGS_LINE( LOG10( XL ), YPOS( I ),
     :                              LOG10( XU ), YPOS( I ) )
                     CALL SGS_LINE( X, YL, X, YU )
                  END IF
               END IF
            END DO

*  Deal with y logarithmic axis.
*  -----------------------------
         ELSE IF ( YLOG ) THEN

*  Loop through the points at the desired frequency.  Do not plot when
*  either co-ordinate is bad.
            DO I = 1, EL, CFREQ
               IF ( XPOS( I ) .NE. VAL__BADR .AND.
     :              YPOS( I ) .NE. VAL__BADR ) THEN

*  Calculate the limits of the errors for the point.  Allow for bad
*  values.
                  IF ( XERROR( I ) .EQ. VAL__BADR ) THEN
                     XL = XPOS( I )
                     XU = XPOS( I )
                  ELSE
                     XL = XPOS( I ) - XERROR( I )
                     XU = XPOS( I ) + XERROR( I )
                  END IF
                  IF ( YERROR( I ) .EQ. VAL__BADR ) THEN
                     YL = YPOS( I )
                     YU = YPOS( I )
                  ELSE
                     YL = YPOS( I ) - YERROR( I )
                     YU = YPOS( I ) + YERROR( I )
                  END IF

*  Check that the y co-ordinates are valid values (able to take a
*  logarithm).
                  IF ( YL .GT. YLOW ) THEN

*  Draw the cross.
                     Y = LOG10( YPOS( I ) )
                     CALL SGS_LINE( XL, Y, XU, Y )
                     CALL SGS_LINE( XPOS( I ), LOG10( YL ),
     :                              XPOS( I ), LOG10( YU ) )
                  END IF
               END IF
            END DO
         END IF

*  Error bars (cross with serifs).
*  ===============================
      ELSE IF ( TYPE( 1:1 ) .EQ. 'B' ) THEN

*  Inquire the zone size.
         CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )

*  Find the size of the tick marks.  These must be the same physical
*  length along each axis, so normalize to an average size of the zone,
*  then convert back into world co-ordinates.
         TICKM = 0.008 * SQRT( XM * YM )
         IF ( XLOG ) THEN
            DX = TICKM / XM * ( LOG10( XMAX ) - LOG10( XMIN ) )
         ELSE
            DX = TICKM / XM * ( XMAX - XMIN )
         END IF
         IF ( YLOG ) THEN
            DY = TICKM / YM * ( LOG10( YMAX ) - LOG10( YMIN ) )
         ELSE
            DY = TICKM / YM * ( YMAX - YMIN )
         END IF

*  Deal with no logarithms first.
*  ------------------------------
         IF ( .NOT. XLOG .AND. .NOT. YLOG ) THEN

*  Loop through the points at the desired frequency.  Do not plot when
*  either co-ordinate is bad.
            DO I = 1, EL, CFREQ
               IF ( XPOS( I ) .NE. VAL__BADR .AND.
     :              YPOS( I ) .NE. VAL__BADR ) THEN

*  Calculate the limits of the errors for the point.  Allow for bad
*  values.
                  IF ( XERROR( I ) .EQ. VAL__BADR ) THEN
                     XL = XPOS( I )
                     XU = XPOS( I )
                  ELSE
                     XL = XPOS( I ) - XERROR( I )
                     XU = XPOS( I ) + XERROR( I )
                  END IF
                  IF ( YERROR( I ) .EQ. VAL__BADR ) THEN
                     YL = YPOS( I )
                     YU = YPOS( I )
                  ELSE
                     YL = YPOS( I ) - YERROR( I )
                     YU = YPOS( I ) + YERROR( I )
                  END IF

*  Draw the cross.
                  CALL SGS_LINE( XL, YPOS( I ), XU, YPOS( I ) )
                  CALL SGS_LINE( XPOS( I ), YL, XPOS( I ), YU )

*  Draw the serifs.
                  CALL SGS_LINE( XL, YPOS( I ) - DY,
     :                           XL, YPOS( I ) + DY )
                  CALL SGS_LINE( XU, YPOS( I ) - DY,
     :                           XU, YPOS( I ) + DY )
                  CALL SGS_LINE( XPOS( I ) - DX, YL,
     :                           XPOS( I ) + DX, YL )
                  CALL SGS_LINE( XPOS( I ) - DX, YU,
     :                           XPOS( I ) + DX, YU )
               END IF
            END DO

*  Deal with both logarithmic axes.
*  --------------------------------
         ELSE IF ( XLOG .AND. YLOG ) THEN

*  Loop through the points at the desired frequency.  Do not plot when
*  either co-ordinate is bad.
            DO I = 1, EL, CFREQ
               IF ( XPOS( I ) .NE. VAL__BADR .AND.
     :              YPOS( I ) .NE. VAL__BADR ) THEN

*  Calculate the limits of the errors for the point.  Allow for bad
*  values.
                  IF ( XERROR( I ) .EQ. VAL__BADR ) THEN
                     XL = XPOS( I )
                     XU = XPOS( I )
                  ELSE
                     XL = XPOS( I ) - XERROR( I )
                     XU = XPOS( I ) + XERROR( I )
                  END IF
                  IF ( YERROR( I ) .EQ. VAL__BADR ) THEN
                     YL = YPOS( I )
                     YU = YPOS( I )
                  ELSE
                     YL = YPOS( I ) - YERROR( I )
                     YU = YPOS( I ) + YERROR( I )
                  END IF

*  Check that these are valid values (able to take a logarithm).
                  IF ( XL .GT. XLOW .AND. YL .GT. YLOW ) THEN

*  Draw the cross.
                     X = LOG10( XPOS( I ) )
                     Y = LOG10( YPOS( I ) )
                     CALL SGS_LINE( LOG10( XL ), Y, LOG10( XU ), Y )
                     CALL SGS_LINE( X, LOG10( YL ), X, LOG10( YU ) )

*  Draw the serifs.  Note that the serif lengths are in terms of the
*  zone co-ordinates, so don't need to take the logarithm of these.
                     CALL SGS_LINE( LOG10( XL ), Y - DY,
     :                              LOG10( XL ), Y + DY )
                     CALL SGS_LINE( LOG10( XU ), Y - DY,
     :                              LOG10( XU ), Y + DY )
                     CALL SGS_LINE( X - DX, LOG10( YL ),
     :                              X + DX, LOG10( YL ) )
                     CALL SGS_LINE( X - DX, LOG10( YU ),
     :                              X + DX, LOG10( YU ) )
                  END IF
               END IF
            END DO

*  Deal with x logarithmic axis.
*  -----------------------------
         ELSE IF ( XLOG ) THEN

*  Loop through the points at the desired frequency.  Do not plot when
*  either co-ordinate is bad.
            DO I = 1, EL, CFREQ
               IF ( XPOS( I ) .NE. VAL__BADR .AND.
     :              YPOS( I ) .NE. VAL__BADR ) THEN

*  Calculate the limits of the errors for the point.  Allow for bad
*  values.
                  IF ( XERROR( I ) .EQ. VAL__BADR ) THEN
                     XL = XPOS( I )
                     XU = XPOS( I )
                  ELSE
                     XL = XPOS( I ) - XERROR( I )
                     XU = XPOS( I ) + XERROR( I )
                  END IF
                  IF ( YERROR( I ) .EQ. VAL__BADR ) THEN
                     YL = YPOS( I )
                     YU = YPOS( I )
                  ELSE
                     YL = YPOS( I ) - YERROR( I )
                     YU = YPOS( I ) + YERROR( I )
                  END IF

*  Check that the x co-ordinates are valid values (able to take a
*  logarithm).
                  IF ( XL .GT. XLOW ) THEN

*  Draw the cross.
                     X = LOG10( XPOS( I ) )
                     CALL SGS_LINE( LOG10( XL ), YPOS( I ),
     :                              LOG10( XU ), YPOS( I ) )
                     CALL SGS_LINE( X, YL, X, YU )

*  Draw the serifs.  Note that the serif lengths are in terms of the
*  zone co-ordinates, so don't need to take the logarithm of these.
                     CALL SGS_LINE( LOG10( XL ), YPOS( I ) - DY,
     :                              LOG10( XL ), YPOS( I ) + DY )
                     CALL SGS_LINE( LOG10( XU ), YPOS( I ) - DY,
     :                              LOG10( XU ), YPOS( I ) + DY )
                     CALL SGS_LINE( X - DX, YL, X + DX, YL )
                     CALL SGS_LINE( X - DX, YU, X + DX, YU )
                  END IF
               END IF
            END DO

*  Deal with y logarithmic axis.
*  -----------------------------
         ELSE IF ( YLOG ) THEN

*  Loop through the points at the desired frequency.  Do not plot when
*  either co-ordinate is bad.
            DO I = 1, EL, CFREQ
               IF ( XPOS( I ) .NE. VAL__BADR .AND.
     :              YPOS( I ) .NE. VAL__BADR ) THEN

*  Calculate the limits of the errors for the point.  Allow for bad
*  values.
                  IF ( XERROR( I ) .EQ. VAL__BADR ) THEN
                     XL = XPOS( I )
                     XU = XPOS( I )
                  ELSE
                     XL = XPOS( I ) - XERROR( I )
                     XU = XPOS( I ) + XERROR( I )
                  END IF
                  IF ( YERROR( I ) .EQ. VAL__BADR ) THEN
                     YL = YPOS( I )
                     YU = YPOS( I )
                  ELSE
                     YL = YPOS( I ) - YERROR( I )
                     YU = YPOS( I ) + YERROR( I )
                  END IF

*  Check that the y co-ordinates are valid values (able to take a
*  logarithm).
                  IF ( YL .GT. YLOW ) THEN

*  Draw the cross.
                     Y = LOG10( YPOS( I ) )
                     CALL SGS_LINE( XL, Y, XU, Y )
                     CALL SGS_LINE( XPOS( I ), LOG10( YL ),
     :                              XPOS( I ), LOG10( YU ) )

*  Draw the serifs.  Note that the serif lengths are in terms of the
*  zone co-ordinates, so don't need to take the logarithm of these.
                     CALL SGS_LINE( XL, Y - DY, XL, Y + DY )
                     CALL SGS_LINE( XU, Y - DY, XU, Y + DY )
                     CALL SGS_LINE( XPOS( I ) - DX, LOG10( YL ),
     :                              XPOS( I ) + DX, LOG10( YL ) )
                     CALL SGS_LINE( XPOS( I ) - DX, LOG10( YU ),
     :                              XPOS( I ) + DX, LOG10( YU ) )
                  END IF
               END IF
            END DO
         END IF
      END IF

*  Flush the plotting buffer.
      CALL SGS_FLUSH

*  Restore the previous current SGS pen.
      CALL SGS_SPEN( PEN )

      END
