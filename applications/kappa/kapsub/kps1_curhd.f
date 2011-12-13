      SUBROUTINE KPS1_CURHD( FRM, MAP, XC, YC, NAX, IAT, LINE, ICOL,
     :                       STATUS )
*+
*  Name:
*     KPS1_CURHD

*  Purpose:
*     Produce a line of column headers for CURSOR.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CURHD( FRM, MAP, XC, YC, NAX, IAT, LINE, ICOL, STATUS )

*  Description:
*     This routine produces a line of column headers that include axis
*     symbols and units. The tabs between columns are sufficient to
*     encompass both headers and formatted axis values.

*  Arguments:
*     FRM = INTEGER (Given)
*        A pointer to the Frame to which the position refers.
*     MAP = INTEGER (Given)
*        A pointer to the Mapping from the supplied graphics position
*        (XC, YC) to the Frame given by FRM.
*     XC = REAL (Given)
*        The X GRAPHICS co-ordinate of a test point (used to determine
*        the minimum tab width between headers).
*     YC = REAL (Given)
*        The Y GRAPHICS co-ordinate of a test point (used to determine
*        the minimum tab width between headers).
*     NAX = INTEGER (Given)
*        The number of axes in FRM.
*     IAT = INTEGER (Given and Returned)
*        The number of characters returned in LINE.
*     LINE = CHARACTER * ( * ) (Given and Returned)
*        The header text.
*     ICOL( NAX ) = INTEGER (Returned)
*        The tab positions for each column of axis values.
*     STATUS = INTEGER (Given)
*        Global status value.

*  Copyright:
*     Copyright (C) 2006 Central Laboratory of the Research Councils.
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
*     DSB: David S. Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     8-JUN-2006 (DSB):
*        Original version.
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

*  Arguments Given and Returned:
      INTEGER IAT
      CHARACTER LINE*(*)

*  Arguments Returned:
      INTEGER ICOL( NAX )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER ATTRIB*20        ! AST attribute name
      CHARACTER FMT*30           ! Formatted axis value
      CHARACTER SYM*30           ! Axis symbol
      CHARACTER UNIT*30          ! Axis Units string
      DOUBLE PRECISION CXY( 2 )  ! Curent Frame position
      DOUBLE PRECISION GXY( 2 )  ! Graphics position
      INTEGER I                  ! Loop count
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

*  Loop round each axis.
      DO I = 1, NAX

*  Store the position of the start of this column.
         ICOL( I ) = IAT

*  Append the axis symbol to the returned text. Form the name of the
*  Symbol attribute for this axis.
         ATTRIB = 'Symbol('
         JAT = 7
         CALL CHR_PUTI( I, ATTRIB, JAT )
         CALL CHR_APPND( ')', ATTRIB, JAT )

*  Get the symbol string.
         SYM = AST_GETC( FRM, ATTRIB( : JAT ), STATUS)

*  Remove any PGPLOT escape sequences.
         CALL KPG1_PGESC( SYM, STATUS )

*  Append the symbol if it is not blank, followed by a space.
         IF( SYM .NE. ' ' ) THEN
            CALL CHR_APPND( SYM, LINE, IAT )
            IAT = IAT + 1
         END IF

*  Append the axis units to the returned text. Form the name of the
*  Unit attribute for this axis.
         ATTRIB = 'Unit('
         JAT = 5
         CALL CHR_PUTI( I, ATTRIB, JAT )
         CALL CHR_APPND( ')', ATTRIB, JAT )

*  Get the unit string.
         UNIT = AST_GETC( FRM, ATTRIB( : JAT ), STATUS)

*  Remove any PGPLOT escape sequences.
         CALL KPG1_PGESC( UNIT, STATUS )

*  Create a "symbol=value" string.
         IF( UNIT .NE. ' ' ) THEN
            IF( SYM .NE. ' ' ) CALL CHR_APPND( '(', LINE, IAT )
            CALL CHR_APPND( UNIT, LINE, IAT )
            IF( SYM .NE. ' ' ) CALL CHR_APPND( ')', LINE, IAT )
         END IF

*  If we have another axis to do, format the test axis value.
         IF( I .NE. NAX ) THEN
            FMT = AST_FORMAT( FRM, I, CXY( I ), STATUS )

*  Increment the position so that this column has room for both a
*  header and a formatted axis value.
            IAT = MAX( IAT - ICOL( I ), CHR_LEN( FMT ) ) + 3 + ICOL( I )
         END IF

      END DO

      END
