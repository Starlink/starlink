      SUBROUTINE CCD1_SGARE( X1, Y1, X2, Y2, AREA, STATUS )

*+
*  Name:
*     CCD1_SGARE

*  Purpose:
*     To calculate the area under a line segment within a unit pixel
*     at the origin.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_SGARE( X1, Y1, X2, Y2, STATUS )

*  Description:
*     This is called from CCD1_BOX, in turn called
*     from CCD1_DODIZ and hence from to DRIZZLE

*  Arguments:
*     X1 = DOUBLE PRECISION (Given)
*       X-coordinate of first point
*     Y1 = DOUBLE PRECISION (Given)
*       Y-coordinate of first point
*     X2 = DOUBLE PRECISION (Given)
*       X-coordinate of second point
*     Y2 = DOUBLE PRECISION (Given)
*       Y-coordinate of second point
*     AREA = DOUBLE PRECISION (Returned)
*       Returned area under the line segment
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Comments:
*     This subroutine is spaghetti code. If it works properly, I'm
*     going to have to come back and fix it for my own peace of mind.
*     {note_any_comments_here}

*  Copyright:
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
*     AA: Alasdair Allan (STARLINK, Keele University)
*     {enter_new_authors_here}

*  History:
*     19-JUL-1999 (AA):
*        Original version based on Bill Sparks BOXER algorithim
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      DOUBLE PRECISION X1, Y1
      DOUBLE PRECISION X2, Y2

*  Arguments Returned:
      DOUBLE PRECISION AREA

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION DX
      DOUBLE PRECISION XLO, XHI, YLO, YHI, XTOP
      DOUBLE PRECISION M, C

      LOGICAL NEGDX

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start to do stuff...
      DX = X2 - X1

*  Trap vertical line
      IF(DX .EQ. 0.0D0 ) THEN
         AREA = 0.0D0
         GOTO 999
      ENDIF

*  Order the two input points
      IF( X1 .LT. X2 ) THEN
         XLO = X1
         XHI = X2
      ELSE
         XLO = X2
         XHI = X1
      ENDIF

*  Determine the bounds (ignore y)
      IF( XLO .GE. 1.0D0 ) THEN
         AREA = 0.0D0
         GOTO 999
      ELSE IF( XHI .LE. 0.0D0 ) THEN
         AREA = 0.0D0
         GOTO 999
      ELSE
         XLO = MAX( XLO, 0.0D0 )
         XHI = MIN( XHI, 1.0D0 )
      ENDIF

*  Work out y = mx + c
      NEGDX = ( DX .LT. 0.0D0 )

      M  = (Y2 - Y1)/DX
      C = Y1 - M * X1
      YLO = M*XLO + C
      YHI = M*XHI + C

*  Trap segment entirely below axis
      IF( YLO .LE. 0.0 .AND. YHI .LE. 0.0 ) THEN
         AREA = 0.0D0
         GOTO 999
      ENDIF

*  Adjust bounds if segment crosses axis (excluding anything below the axis)
      IF( YLO .LT. 0.0 ) THEN
         YLO = 0.0D0
         XLO = -C/M
      ENDIF

      IF( YHI .LT. 0.0 ) THEN
         YHI = 0.0D0
         XHI = -C/M
      ENDIF

*  There are four possibilities: both y below 1; both y below 1; one of each
      IF( YLO .GE. 1.0D0 .AND. YHI .GE. 1.0D0 ) THEN

*  Line segment is entirely above square
         IF( NEGDX ) THEN
            AREA = XLO - XHI
         ELSE
            AREA = XHI - XLO
         ENDIF

      ELSE IF( YLO .LE. 1.0D0 .AND. YHI .LE. 1.0D0 ) THEN

*  Segment is entirely within square
         IF( NEGDX ) THEN
            AREA = 0.5D0*( XLO - XHI )*( YHI + YLO )
         ELSE
            AREA = 0.5D0*( XHI - XLO )*( YHI + YLO )
         ENDIF

      ELSE

*  Otherwise it must cross the top of the square
         XTOP = ( 1.0D0 - C )/M

         IF( XTOP .LT. XLO-1.0D-5
     :       .OR. XTOP .GT. XHI+1.0D-5 ) THEN

*  Check for problems, include an error margin so that if we
*  have numerical rounding errors we'll still pass the test
            STATUS = SAI__ERROR
            GOTO 999
         ENDIF

         IF( YLO .LT. 1.0 ) THEN

            IF( NEGDX ) THEN
               AREA = - ( 0.5D0*( XTOP - XLO )*
     :                  ( 1.0D0 + YLO ) + XHI - XTOP )
            ELSE
               AREA = 0.5D0*( XTOP - XLO )*( 1.0D0 + YLO )
     :                + XHI - XTOP
            ENDIF

         ELSE

            IF( NEGDX ) THEN
               AREA = - ( 0.5D0*( XHI - XTOP )*
     :                  ( 1.0D0 + YHI ) + XTOP - XLO )
            ELSE
               AREA = 0.5D0*( XHI - XTOP )*( 1.0D0 + YHI )
     :                + XTOP - XLO
            ENDIF

         ENDIF

      ENDIF

*  Time at the bar please...
999   END
