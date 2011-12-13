      SUBROUTINE KPS1_CURFM( FRM, MAP, XC, YC, NAX, ICOL, IAT, LINE,
     :                       GOOD, CXY, STATUS )
*+
*  Name:
*     KPS1_CURFM

*  Purpose:
*     Format a position for CURSOR.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CURFM( FRM, MAP, XC, YC, NAX, ICOL, IAT, LINE, GOOD,
*                      CXY, STATUS )

*  Description:
*     This routine formats a position for application CURSOR. The
*     position is supplied in GRAPHICS co-ordinates. It is mapped into
*     the required Frame before being formatted.

*  Arguments:
*     FRM = INTEGER (Given)
*        A pointer to the Frame to which the position refers.
*     MAP = INTEGER (Given)
*        A pointer to the Mapping from the supplied graphics position
*        (XC, YC) to the Frame given by FRM.
*     XC = REAL (Given)
*        The X GRAPHICS co-ordinate.
*     YC = REAL (Given)
*        The Y GRAPHICS co-ordinate.
*     NAX = INTEGER (Given)
*        The number of axes in FRM.
*     ICOL( NAX ) = INTEGER (Given)
*        The tab positions for each column of axis values.
*     IAT = INTEGER (Given and Returned)
*        The number of characters in LINE.
*     LINE = CHARACTER * ( * ) (Given and Returned)
*        The text.
*     GOOD = LOGICAL (Returned)
*        Were all axis value good in Frame FRM?
*     CXY( NAX ) = DOUBLE PRECISION (Returned)
*        The corresponding co-ordinates in FRM.
*     STATUS = INTEGER (Given)
*        Global status value.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-OCT-1998 (DSB):
*        Original version.
*     26-MAY-2006 (DSB):
*        Correct tabbing between output columns.
*     8-JUN-2006 (DSB):
*        Modified so that it never includes axis labels and units.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      INTEGER FRM
      INTEGER MAP
      REAL XC
      REAL YC
      INTEGER NAX
      INTEGER ICOL( NAX )

*  Arguments Given and Returned:
      INTEGER IAT
      CHARACTER LINE*(*)

*  Arguments Returned:
      LOGICAL GOOD
      DOUBLE PRECISION CXY( NAX )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER FMT*30           ! Formatted axis value
      DOUBLE PRECISION GXY( 2 )  ! Graphics position
      INTEGER I                  ! Loop count
      INTEGER INDENT             ! Indent from start of line
      INTEGER JAT                ! No. of characters in the string
*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Transform the cursor position from GRAPHICS co-ordinates into the
*  required Frame.
      GXY( 1 ) = DBLE( XC )
      GXY( 2 ) = DBLE( YC )
      CALL AST_TRANN( MAP, 1, 2, 1, GXY, .TRUE., NAX, 1, CXY, STATUS )

*  Normalise the position.
      CALL AST_NORM( FRM, CXY, STATUS )

*  Assume all axis values are good in the required Frame.
      GOOD = .TRUE.

*  Note the initial indent.
      INDENT = IAT

*  Loop round each axis.
      DO I = 1, NAX

*  Go to the start of the column for this axis, or to the next tab stop
*  if we have already passed the column for this axis.
         IF( IAT .LE. ICOL( I ) + INDENT ) THEN
            IAT = ICOL( I ) + INDENT
         ELSE
            IAT = 6*( 1 + IAT/6 )
         END IF

*  Are all axis values good?
         IF( CXY( I ) .EQ. AST__BAD ) GOOD = .FALSE.

*  Format and append the axis value.
         FMT = AST_FORMAT( FRM, I, CXY( I ), STATUS )
         CALL CHR_APPND( FMT, LINE, IAT )

*  Add on a trailing space.
         IF( I .NE. NAX ) IAT = IAT + 1

      END DO

      END
