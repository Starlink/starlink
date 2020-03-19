      SUBROUTINE KPG1_DRFIT( N, X, Y, XLO, XHI, YLO, YHI, IPLOT, SLOPE,
     :                       OFFSET, RMS, STATUS )
*+
*  Name:
*     KPG1_DRFIT

*  Purpose:
*     Calculate and draw a linear fit to a set of X and Y values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_DRFIT( N, X, Y, XLO, XHI, YLO, YHI, IPLOT, SLOPE, OFFSET,
*                      RMS, STATUS )

*  Description:
*     This routine does a symetric least squares linear fit to the supplied
*     X and Y values, draws it on the supplied plot, and returns the
*     slope, offset and RMS residual. Outliers are identified and removed.
*     See KPG1_SYMFIT for more information about how the fit is done.

*  Arguments:
*     N = INTEGER*8 (Given)
*        Number of points
*     X( N ) = REAL (Given)
*        X value at each point.
*     Y( N ) = REAL (Given)
*        Y value at each point.
*        suppresses error bars. Otherwise error bars are drawn which extend
*        by from Y - NSIGMA*YSIGMA to Y + NSIGMA*YSIGMA.
*     XLO = REAL (Given)
*        The lowest X value to include in the fit. See also XHI.
*     XHI = REAL (Given)
*        The highest X value to include in the fit. All X values are used
*        if XHI is less than or equal to XLO.
*     YLO = REAL (Given)
*        The lowest Y value to include in the fit. See also YHI.
*     YHI = REAL (Given)
*        The highest Y value to include in the fit. All Y values are used
*        if YHI is less than or equal to YLO.
*     IPLOT = INTEGER (Given)
*        An AST Plot that can be used to draw the line. No line is drawn
*        if this is AST__NULL, but the parameters of the line are still
*        calculated and returned.
*     SLOPE = DOUBLE PRECISION (Returned)
*        The slope of the linear fit: Y = SLOPE * X + OFFSET.
*     OFFSET = DOUBLE PRECISION (Returned)
*        The offset of the linear fit: Y = SLOPE * X + OFFSET.
*     RMS = DOUBLE PRECISION (Returned)
*        The RMS residual of the data (excluding outliers) about the
*        linear fit.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2020 East Asian Observatory
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: D.S. Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     18-MAR-2020 (DSB):
*        Original version.
*     19-MAR-2020 (DSB):
*        Added arguments XLO, XHI, YLO and YHI.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      INTEGER*8 N
      REAL X( N )
      REAL Y( N )
      REAL XLO
      REAL XHI
      REAL YLO
      REAL YHI
      INTEGER IPLOT

*  Arguments Returned:
      DOUBLE PRECISION SLOPE
      DOUBLE PRECISION OFFSET
      DOUBLE PRECISION RMS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION END( 2 )
      DOUBLE PRECISION GX( 2 )
      DOUBLE PRECISION GY( 2 )
      DOUBLE PRECISION START( 2 )
      DOUBLE PRECISION WX( 2 )
      DOUBLE PRECISION WY( 2 )
      INTEGER OLDCLIP
      REAL XL
      REAL XR
      REAL YB
      REAL YT
*.

*  Check inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Calculate the fit parameters.
      CALL KPG1_SYMFITR( N, X, Y, XLO, XHI, YLO, YHI, .TRUE.,
     :                   SLOPE, OFFSET, RMS, STATUS )

*  If required, draw the fit.
      IF( IPLOT .NE. AST__NULL .AND. SLOPE .NE. VAL__BADD ) THEN

*  Get the bounds of the current PGPLOT window.
         CALL PGQWIN( XL, XR, YB, YT )

*  Transform them into the current Frame of the Plot.
         GX( 1 ) = XL
         GX( 2 ) = XR
         GY( 1 ) = YB
         GY( 2 ) = YT
         CALL AST_TRAN2( IPLOT, 2, GX, GY, .TRUE., WX, WY, STATUS )

*  Find the Y value at which the line crosses the left and right hand
*  edges of the plot and so set the start and end points of the line to
*  be drawn.
         START( 1 ) = WX( 1 )
         START( 2 ) = WX( 1 )*SLOPE + OFFSET
         END( 1 ) = WX( 2 )
         END( 2 ) = WX( 2 )*SLOPE + OFFSET

*  Ensure curves are clipped at the boundary of the Plot.
         IF( AST_TEST( IPLOT, 'Clip', STATUS ) ) THEN
            OLDCLIP = AST_GETI( IPLOT, 'Clip', STATUS )
         ELSE
            OLDCLIP = -1
         END IF
         CALL AST_SETI( IPLOT, 'Clip', 1, STATUS )

*  Plot the curve.
         CALL AST_CURVE( IPLOT, START, END, STATUS )

*  Re-instate the original value of the Clip attribute in the Plot.
         IF( OLDCLIP .NE. -1 ) THEN
            CALL AST_SETI( IPLOT, 'Clip', OLDCLIP, STATUS )
         ELSE
            CALL AST_CLEAR( IPLOT, 'Clip', STATUS )
         END IF
      END IF

      END
