      SUBROUTINE KPG1_PGTXT( ANGLE, TEXT, X, Y, STATUS )
*+
*  Name:
*     KPG1_PGTXT

*  Purpose:
*     Draws text using PGPLOT and return concatenation point.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PGTXT( ANGLE, TEXT, X, Y, STATUS )

*  Description:
*     This routine plots the supplied text at a given angle using
*     PGPLOT, putting the bottom left corner of the text at the
*     supplied position. The position at which another string must be
*     drawn to concatenate it with the string just drawn is returned.

*  Arguments:
*     ANGLE = INTEGER (Given)
*        The angle, in degrees, that the baseline is to make with the
*        horizontal, increasing anti-clockwise (0.0 is horizontal).
*     TEXT = CHARACTER * ( * ) (Given)
*        The text to draw. The returned values of X and Y leave room for
*        any trailing spaces, UNLESS THE ENTIRE STRING IS BLANK, IN WHICH
*        CASE X AND Y ARE RETURNED UNCHANGED.
*     X = REAL (Given and Returned)
*        The X position for the bottom-left corner of the string.
*        On exit, it is the X position at the bottom of the a string
*        to be concatenated to the one just drawn.
*     Y = REAL (Given and Returned)
*        The Y position for the bottom-left corner of the string.
*        On exit, it is the Y position at the bottom of the a string
*        to be concatenated to the one just drawn.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-MAR-1998 (DSB):
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
      REAL ANGLE
      CHARACTER TEXT*(*)

*  Arguments Given and Returned:
      REAL X
      REAL Y

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN

*  Local variables :
      REAL XBOX( 4 ), YBOX( 4 ), DX, DY, FACT
*.

*  Check the inherited status. Return if the text string is blank.
      IF ( STATUS .NE. SAI__OK .OR. TEXT .EQ. ' ' ) RETURN

*  Draw the string
      CALL PGPTXT( X, Y, ANGLE, 0.0, TEXT )

*  Find the bounding box of the string just drawn.
      CALL PGQTXT( X, Y, ANGLE, 0.0, TEXT, XBOX, YBOX )

*  Find the offset from the start to the end of the bounding box.
      DX = XBOX( 4 ) - XBOX( 1 )
      DY = YBOX( 4 ) - YBOX( 1 )

*  Make this longer to include any trailing spaces in the supplied text.
*  (PGPLOT ignores trailing spaces when plotting text).
      IF( TEXT .NE. ' ' ) THEN
         FACT = REAL( LEN( TEXT ) )/REAL( CHR_LEN( TEXT ) )
      ELSE
         FACT = 1.0
      END IF

*  Return the concatenation point.
      X = X + FACT*DX
      Y = Y + FACT*DY


      END
