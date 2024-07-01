      SUBROUTINE ASTANGLE( STATUS )
*+
*  Name:
*     ASTANGLE

*  Purpose:
*     Calculate the angle subtended by two points at a third point

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTANGLE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application finds the angle at point B between the line
*     joining points A and B, and the line joining points C and B.
*     These lines will in fact be geodesic curves appropriate to the
*     Frame in use. For instance, in SkyFrame, they will be great
*     circles.

*  Usage:
*     astangle this a b c

*  ADAM Parameters:
*     A() = _DOUBLE (Read)
*        An array with one element for each Frame axis (Naxes attribute)
*        containing the coordinates of the first point.
*     ANGLE = _DOUBLE (Write)
*        The calculated angle.  It is radians unless DEGS is TRUE.
*     B() = _DOUBLE (Read)
*        An array with one element for each Frame axis (Naxes attribute)
*        containing the coordinates of the second point.
*     C() = _DOUBLE (Read)
*        An array with one element for each Frame axis (Naxes attribute)
*        containing the coordinates of the third point.
*     DEGS = _LOGICAL (Read)
*        If TRUE, the angle is reported in degrees. Otherwise it is
*        reported in radians. [FALSE]
*     THIS = LITERAL (Read)
*        An NDF, FITS file or text file holding the Frame. If an NDF is
*        supplied, the current Frame of the WCS FrameSet will be used. If a
*        FITS file is supplied, the Frame corresponding to the primary axis
*        descriptions will be used.

*  Copyright:
*     Copyright (C) 2011, 2024 Science & Technology Facilities Council.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-SEP-2011 (DSB):
*        Original version.
*     2024 July 1 (MJC):
*        Write calculated angle to an output parameter.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  External References:
      EXTERNAL AST_ISAFRAME

*  Status:
      INTEGER STATUS

*  Local Variables:
      DOUBLE PRECISION ANG
      DOUBLE PRECISION A( NDF__MXDIM )
      DOUBLE PRECISION B( NDF__MXDIM )
      DOUBLE PRECISION C( NDF__MXDIM )
      INTEGER THIS, NC
      LOGICAL DEGS
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the Frame.
      CALL KPG1_GTOBJ( 'THIS', 'Frame', AST_ISAFRAME, THIS,
     :                 STATUS )

*  Get the number of axes in the Frame
      NC = AST_GETI( THIS, 'Naxes', STATUS )

*  Get the three points.
      CALL PAR_EXACD( 'A', NC, A, STATUS )
      CALL PAR_EXACD( 'B', NC, B, STATUS )
      CALL PAR_EXACD( 'C', NC, C, STATUS )

*  Get the angle.
      ANG = AST_ANGLE( THIS, A, B, C, STATUS )

*  If required, convert to degeres.
      CALL PAR_GET0L( 'DEGS', DEGS, STATUS )
      IF( DEGS ) ANG = ANG*AST__DR2D

*  Display the result
      IF( ANG .NE. AST__BAD ) THEN
         CALL MSG_SETD( 'D', ANG )
      ELSE
         CALL MSG_SETC( 'D', '<bad>' )
      END IF

      CALL MSG_OUT( ' ', '^D', STATUS )
      CALL PAR_PUT0D( 'ANGLE', ANG, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTANGLE_ERR', 'Error finding the angle '//
     :                 'subtended by two points at a third point.',
     :                  STATUS )
      END IF

      END
