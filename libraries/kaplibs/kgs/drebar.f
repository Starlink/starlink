      SUBROUTINE DREBAR( X, Y, ERROR, NPTS, STATUS )
*+
*  Name:
*     DREBAR

*  Purpose:
*     Draws a series of error bars over an AUTOGRAPH background.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DREBAR( X, Y, ERROR, NPTS, STATUS )

*  Description:
*     Draws a diamond centred on each point, then draws a vertical
*     line through the diamond. It assumes that the current SGS zone on
*     entry covers the AUTOGRAPH grid window and has world coordinates
*     corresponding to user coordinates.

*  Arguments:
*     X( NPTS ) = REAL (Given)
*        The x coordinate of the centre of each error bar, given in user
*        x coordinates.
*     Y( NPTS ) = REAL (Given)
*        The y coordinate of the centre of each error bar, given in user
*        y coordinates.
*     ERROR( NPTS ) = REAL (Given)
*        The length of each error bar, given in user y coordinates.
*     NPTS = INTEGER (Given)
*        The number of error bars to plot.
*     STATUS = INTEGER (Given and returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-JUN-1990 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing


*  Global Constants:

      INCLUDE 'SAE_PAR'          ! Standard SAE constants


*  Arguments Given:

      INTEGER  NPTS
      REAL     X( NPTS )
      REAL     Y( NPTS )
      REAL     ERROR( NPTS )


*  Status:

      INTEGER STATUS             ! Global status


*  Local Variables:

      REAL      DX               ! Half width of a diamond in user
                                 ! co-ordinates
      REAL      DY               ! Half height of a diamond in user
                                 ! co-ordinates
      INTEGER   I                ! Loop count
      REAL      TX( 5 )          ! x coords of diamond vertices
      REAL      TY( 5 )          ! y coords of diamond vertices
      REAL      X1               ! Lower limit of current zone in x
      REAL      X2               ! Upper limit of current zone in x
      REAL      XM               ! x size of current zone in metres
      REAL      XX               ! Current x position
      REAL      Y1               ! Lower limit of current zone in y
      REAL      Y2               ! Upper limit of current zone in y
      REAL      YM               ! y size of current zone in metres
      REAL      YY               ! Current y position

*.

*  Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN


*  Find the extent of the current SGS zone (assumed to cover the
*  AUTOGRAPH grid window).

      CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )


*  Calculate a size for the diamonds in user co-ords.

      DX = 0.004 * ( X2 - X1 )
      DY = 0.0066 * ( Y2 - Y1 )


*  Loop round each point.

      DO I = 1, NPTS

*  Store current position.

         XX = X( I )
         YY = Y( I )

*  Set up coords of diamond vertices, centred on current coords.

         TX( 1 ) = XX
         TX( 2 ) = XX- DX
         TX( 3 ) = XX
         TX( 4 ) = XX+ DX
         TX( 5 ) = XX
         TY( 1 ) = YY - DY
         TY( 2 ) = YY
         TY( 3 ) = YY + DY
         TY( 4 ) = YY
         TY( 5 ) = YY - DY


*  Draw the diamond.

         CALL GPL( 5, TX, TY )


*  Draw the error bar as a vertical straight line.

         IF( ERROR( I ) .GT. DY ) THEN
            CALL SGS_LINE( XX, YY - ERROR( I ), XX, YY + ERROR( I ) )

         ELSE
            CALL SGS_LINE( XX, YY, XX, YY + DY )

         END IF

      END DO


*  Flush graphics buffers.

      CALL SGS_FLUSH


      END

